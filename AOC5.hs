module AOC5 where

import           Control.Monad
import           Data.Char
import           Data.List
import           Text.Parsec
import           Text.Parsec.ByteString (Parser, parseFromFile)

data Polarity
  = Up
  | Down
  deriving (Eq, Show)

data Unit = Unit
  { _type :: Char
  , _pol  :: Polarity
  } deriving (Eq)

sameType :: Unit -> Unit -> Bool
sameType u u' = _type u == _type u'

samePol :: Unit -> Unit -> Bool
samePol u u' = _pol u == _pol u'

reacts :: Unit -> Unit -> Bool
reacts u = liftM2 (&&) (sameType u) (not . samePol u)

hardTypeFilter :: Char -> Unit -> Bool
hardTypeFilter t = (/= t) . _type

instance Show Unit where
  show (Unit c Up)   = show $ toUpper c
  show (Unit c Down) = show $ toLower c

newtype Polymer =
  Polymer [Unit]
  deriving (Eq, Show)

instance Semigroup Polymer where
  (<>) = mappend

instance Monoid Polymer where
  mempty = Polymer []
  mappend (Polymer p) (Polymer p') = Polymer (mappend p p')

unitCount :: Polymer -> Int
unitCount (Polymer p) = length p

unitParser :: Parser Unit
unitParser = do
  l <- letter
  pure $
    if isUpper l
      then Unit (toLower l) Up
      else Unit (toLower l) Down

polymerParser :: Parser Polymer
polymerParser = Polymer <$> many unitParser

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

--input = fromRight mempty <$> parseFromFile (polymerParser) "example.txt"
input = fromRight mempty <$> parseFromFile polymerParser "AOC5.input"

type Reacted = Bool

reactRound :: Polymer -> (Reacted, Polymer)
reactRound (Polymer []) = (False, Polymer [])
reactRound (Polymer units) = Polymer <$> foldr reaction (False, []) units
  where
    reaction :: Unit -> (Reacted, [Unit]) -> (Reacted, [Unit])
    reaction u (r, []) = (r, [u])
       --reaction u (True, us) = (True, u:us)
    reaction u (reacted, u':us) =
      if reacts u u'
        then (True, us)
        else (reacted, u : u' : us)

react :: (Reacted, Polymer) -> Polymer
react (False, p) = p
react (True, p)  = react $ reactRound p

filterP :: Polymer -> (Unit -> Bool) -> Polymer
filterP (Polymer us) p = Polymer $ filter p us

-- 10888
solution1 = do
  polymer <- input
  let reduced = react (True, polymer)
  pure $ unitCount reduced

-- 6952
solution2 = do
  polymer <- input
  let candidates = hardTypeFilter <$> ['a' .. 'z']
  let ps =
        zip ['a' .. 'z'] $
        unitCount . react . (,) True . filterP polymer <$> candidates
  let badPolymer = snd . head . sortOn snd $ ps
  pure badPolymer
