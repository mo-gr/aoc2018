{-# LANGUAGE NamedFieldPuns #-}

module AOC24 where

import           Data.List
import           Text.Parsec
import           Text.Parsec.ByteString (Parser, parseFromFile)

data Attack
  = Fire
  | Ice
  | Slash
  | Bludgeon
  | Radiation
  deriving (Eq, Show)

type HP = Int

type AttackPoints = Int

type Initiative = Int

data Group = Group
  { units      :: Int
  , hitPoints  :: HP
  , attack     :: Attack
  , damage     :: AttackPoints
  , initiative :: Initiative
  , weak       :: [Attack]
  , immune     :: [Attack]
  }

instance Show Group where
  show g =
    "Group " ++
    show (initiative g) ++ " contains " ++ show (units g) ++ " units"

instance Eq Group where
  g == g' = (initiative g) == (initiative g')

instance Ord Group where
  compare g g' =
    case compare (effectivePower g) (effectivePower g') of
      EQ -> compare (initiative g) (initiative g')
      x  -> x

data Strike = Strike
  { strikePower :: AttackPoints
  , attacker    :: Group
  , target      :: Group
  } deriving (Eq)

instance Show Strike where
  show (Strike p a t) =
    "Strike " ++
    show (initiative a) ++ "->" ++ show (initiative t) ++ " @ " ++ show p

instance Ord Strike where
  compare s s' =
    case compare (strikePower s) (strikePower s') of
      EQ ->
        case compare
               (effectivePower . target $ s)
               (effectivePower . target $ s') of
          EQ -> compare (target s) (target s')
          x  -> x
      x -> x

data BattleField = BattleField
  { infection    :: [Group]
  , immuneSystem :: [Group]
  } deriving (Show)

effectivePower :: Group -> AttackPoints
effectivePower g = (damage g) * (units g)

isImmune :: Group -> Attack -> Bool
isImmune (Group {immune = gi}) sa = any (== sa) gi

isWeak :: Group -> Attack -> Bool
isWeak (Group {weak = gw}) sa = any (== sa) gw

strike :: Group -> Group -> Strike
strike attacker defender =
  let attackType = (attack attacker)
      damageMulti =
        if isWeak defender attackType
          then (* 2)
          else if isImmune defender attackType
                 then (* 0)
                 else (* 1)
      damagePower = damageMulti (effectivePower attacker)
   in Strike damagePower attacker defender

performStrike :: Strike -> Maybe Group
performStrike s =
  let g = (target s)
      damagePower = strikePower s
      dead = damagePower `div` (hitPoints g)
      unitsAfterAttack = (units g) - dead
   in if unitsAfterAttack <= 0
        then Nothing
        else pure $ g {units = unitsAfterAttack}

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe a  = Just $ head a

select :: Group -> [Group] -> Maybe Strike
select g targets =
  headMaybe . reverse . sort . filter ((> 0) . strikePower) $
  (strike g <$> targets)

dropTarget :: Strike -> [Group] -> [Group]
dropTarget (Strike {target = t}) g = filter (/= t) g

targetSelection :: BattleField -> [Strike]
targetSelection bf =
  let attackers = reverse . sort $ infection bf
      defenders = reverse . sort $ immuneSystem bf
      ts (strikes, defenders) a =
        case select a defenders of
          Just s  -> (s : strikes, dropTarget s defenders)
          Nothing -> (strikes, defenders)
   in sortOn (initiative . attacker) $
      (fst $ foldl ts ([], defenders) attackers) ++
      (fst $ foldl ts ([], attackers) defenders)

find' :: Eq a => a -> [a] -> a
find' a' [] = error "should have found a'"
find' a' (a:aa)
  | a' == a = a
  | otherwise = find' a' aa

findArmies ::
     BattleField
  -> Strike
  -> (Maybe (Strike, [Group]), Maybe ([Group] -> BattleField))
findArmies bf s
  | elem (attacker s) (infection bf) && elem (target s) (immuneSystem bf) =
    let strike' =
          strike
            (find' (attacker s) (infection bf))
            (find' (target s) (immuneSystem bf))
     in (Just (strike', immuneSystem bf), Just $ updateImmune bf)
  | elem (attacker s) (immuneSystem bf) && elem (target s) (infection bf) =
    let strike' =
          strike
            (find' (attacker s) (immuneSystem bf))
            (find' (target s) (infection bf))
     in (Just (strike', infection bf), Just $ updateInfection bf)
  | otherwise = (Nothing, Nothing)

updateInfection :: BattleField -> [Group] -> BattleField
updateInfection bf g = bf {infection = g}

updateImmune :: BattleField -> [Group] -> BattleField
updateImmune bf g = bf {immuneSystem = g}

updateArmies :: ([Group] -> BattleField) -> [Group] -> Strike -> BattleField
updateArmies up g s =
  let t = (target s)
   in case performStrike s of
        Nothing -> up (filter (/= t) g)
        Just survivor ->
          up
            (map
               (\x ->
                  if x == t
                    then survivor
                    else x)
               g)

performAttack :: BattleField -> Strike -> BattleField
performAttack bf strike =
  case findArmies bf strike of
    (Just (strike', defender), Just updater) ->
      updateArmies updater defender strike'
    _ -> bf

peace :: BattleField -> Bool
peace (BattleField [] _) = True
peace (BattleField _ []) = True
peace _                  = False

battle :: BattleField -> BattleField
battle bf =
  let strikes = targetSelection bf
   in foldl performAttack bf $ reverse strikes

-- Parser
number :: Parser Int
number = read <$> many1 digit <* many space

attackTypeParser :: Parser Attack
attackTypeParser =
  do string "radiation" *> return Radiation
     <|> string "bludgeoning" *> return Bludgeon <|>
  string "fire" *> return Fire <|>
  string "cold" *> return Ice <|>
  string "slashing" *> return Slash

specialTratesParser :: Parser ([Attack], [Attack])
specialTratesParser = do
  _ <- string "("
  immune <-
    (string "immune to " *> (attackTypeParser `sepBy` (string ", "))) <|>
    return []
  _ <- string "; " <|> return []
  weak <-
    (string "weak to " *> (attackTypeParser `sepBy` (string ", "))) <|>
    return []
  _ <- string "; " <|> return []
  immune' <-
    (string "immune to " *> (attackTypeParser `sepBy` (string ", "))) <|>
    return []
  _ <- string ")" <* many space
  pure (immune ++ immune', weak)

groupParser :: Parser Group
groupParser = do
  unitCount <- number <* string "units each with "
  hitPoints <- number <* string "hit points "
  (immune, weak) <- specialTratesParser <|> return ([], [])
  damage <- string "with an attack that does " *> number
  attackType <- attackTypeParser <* string " damage "
  initiative <- string "at initiative " *> number
  pure $
    Group
      { units = unitCount
      , hitPoints = hitPoints
      , attack = attackType
      , damage = damage
      , initiative = initiative
      , weak = weak
      , immune = immune
      }

battleFieldParser :: Parser BattleField
battleFieldParser = do
  immune <-
    string "Immune System:" *> many space *> many groupParser <* many space
  infect <- string "Infection:" *> many space *> many groupParser <* many space
  pure $ BattleField {immuneSystem = immune, infection = infect}

input = parseFromFile (battleFieldParser) "AOC24.input"

input' = parseFromFile (battleFieldParser) "example.txt"

fromRight :: Show a => Either a b -> b
fromRight (Right b) = b
fromRight (Left e)  = error $ show e

debugUntil :: Show a => (a -> Bool) -> (a -> a) -> a -> IO a
debugUntil cond f a
  | cond a = do
    print a
    pure a
  | otherwise = do
    let a' = f a
    print a
    debugUntil cond f a'

value :: BattleField -> Int
value (BattleField a b) = sum (units <$> a) + sum (units <$> b)

boost :: BattleField -> Int -> BattleField
boost bf b =
  bf {immuneSystem = (\g -> g {damage = b + damage g}) <$> immuneSystem bf}

-- WRONG 21760 too low
-- CORRECT 21765
solution1 = do
  bf <- fromRight <$> input
  --debugUntil peace battle bf
  let aftermath = until peace battle bf
  pure . value $ aftermath

-- CORRECT 5522
solution2 = do
  bf <- fromRight <$> input
  let bf' = boost bf 119
  --debugUntil peace battle bf
  let aftermath = until peace battle bf'
  pure . value $ aftermath
