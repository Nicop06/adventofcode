module Day24
  ( parseInput,
    part1,
    part2,
  )
where

import qualified Data.IntMap as M
import Data.List (find, maximumBy, nub, sortOn)
import Data.Ord (Down (..), comparing)
import Text.Parsec
import Text.Parsec.String

data ModifierType = Immunity | Weakness deriving (Show)

type Modifier = (ModifierType, String)

data Army = ImmuneSystem | Infection deriving (Show, Eq)

data ArmyGroup = ArmyGroup
  { _army :: Army,
    _numUnits :: Int,
    _hitPoints :: Int,
    _modifiers :: [Modifier],
    _attackDamage :: Int,
    _damageType :: String,
    _initiative :: Int
  }
  deriving (Show)

type ArmyGroups = M.IntMap ArmyGroup

isEnemy :: ArmyGroup -> ArmyGroup -> Bool
isEnemy g1 g2 = _army g1 /= _army g2

effectivePower :: ArmyGroup -> Int
effectivePower g = _attackDamage g * _numUnits g

damage :: ArmyGroup -> ArmyGroup -> Int
damage attacker defender =
  case find ((== _damageType attacker) . snd) (_modifiers defender) of
    Nothing -> effectivePower attacker
    Just (Immunity, _) -> 0
    Just (Weakness, _) -> 2 * effectivePower attacker

attack :: ArmyGroup -> ArmyGroup -> Maybe ArmyGroup
attack attacker defender =
  let numUnits = (_numUnits defender - (damage attacker defender `div` _hitPoints defender))
   in if numUnits <= 0 then Nothing else Just defender {_numUnits = numUnits}

targetSelection :: ArmyGroups -> M.IntMap Int
targetSelection armies =
  M.fromList $
    go
      (sortOn (targetSelectionOrdering . snd) (M.assocs armies))
      armies
  where
    go :: [(Int, ArmyGroup)] -> ArmyGroups -> [(Int, Int)]
    go [] _ = []
    go ((i, attacker) : gs) groups =
      let enemies = M.filter (isEnemy attacker) groups
       in if null enemies
            then go gs groups
            else
              let (t, defender) = maximumBy (comparing (targetOrdering attacker . snd)) (M.assocs enemies)
               in if damage attacker defender > 0
                    then (i, t) : go gs (M.delete t groups)
                    else go gs groups

    targetSelectionOrdering :: ArmyGroup -> Down (Int, Int)
    targetSelectionOrdering g = Down (effectivePower g, _initiative g)

    targetOrdering :: ArmyGroup -> ArmyGroup -> (Int, Int, Int)
    targetOrdering attacker defender =
      ( damage attacker defender,
        effectivePower defender,
        _initiative defender
      )

runFight :: ArmyGroups -> ArmyGroups
runFight armies =
  foldl (flip applyAttack) armies orderedAttackers
  where
    selectedTargets :: M.IntMap Int
    selectedTargets = targetSelection armies

    orderedAttackers :: [Int]
    orderedAttackers = map fst $ sortOn (Down . _initiative . snd) (M.assocs armies)

    applyAttack :: Int -> ArmyGroups -> ArmyGroups
    applyAttack i groups = case M.lookup i groups of
      Nothing -> groups
      Just attacker -> case M.lookup i selectedTargets of
        Nothing -> groups
        Just t -> M.update (attack attacker) t groups

combat :: ArmyGroups -> ArmyGroups
combat groups
  | length (nub $ map _army $ M.elems groups) == 1 = groups
  | otherwise =
      let fightResult = runFight groups
       in if numUnits fightResult == numUnits groups
            then groups
            else combat fightResult
  where
    numUnits :: ArmyGroups -> Int
    numUnits = sum . M.map _numUnits

parseInput :: Parser ArmyGroups
parseInput =
  M.fromList . zip [1 ..]
    <$> ( (++)
            <$> (string "Immune System:\n" *> parseArmyGroup ImmuneSystem `sepEndBy1` newline)
            <*> (string "\nInfection:\n" *> parseArmyGroup Infection `sepEndBy1` newline)
            <* eof
        )
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 digit

    parseModifiers :: Parser [Modifier]
    parseModifiers = concat <$> ((parseImmunity <|> parseWeakness) `sepBy1` string "; ")
      where
        parseModifier :: String -> Parser [String]
        parseModifier m = string (m ++ " to ") *> (many1 alphaNum `sepBy1` string ", ")

        parseImmunity :: Parser [Modifier]
        parseImmunity = map (Immunity,) <$> parseModifier "immune"

        parseWeakness :: Parser [Modifier]
        parseWeakness = map (Weakness,) <$> parseModifier "weak"

    parseArmyGroup :: Army -> Parser ArmyGroup
    parseArmyGroup t =
      ArmyGroup t
        <$> (parseInt <* string " units each with ")
        <*> (parseInt <* string " hit points ")
        <*> (try (between (char '(') (string ") ") parseModifiers) <|> pure [])
        <*> between (string "with an attack that does ") (char ' ') parseInt
        <*> (many1 alphaNum <* string " damage at initiative ")
        <*> parseInt

part1 :: ArmyGroups -> IO ()
part1 = print . sum . M.map _numUnits . combat

part2 :: ArmyGroups -> IO ()
part2 groups = print . sum . M.map _numUnits $ go 0
  where
    winningArmy :: ArmyGroups -> Maybe Army
    winningArmy g = case nub . map _army . M.elems $ g of
      [a] -> Just a
      _ -> Nothing

    go :: Int -> ArmyGroups
    go b =
      let result = combat (M.map (boostImuneSystem b) groups)
       in case winningArmy result of
            Just ImmuneSystem -> result
            _ -> go (b + 1)

    boostImuneSystem :: Int -> ArmyGroup -> ArmyGroup
    boostImuneSystem amount g = case _army g of
      Infection -> g
      ImmuneSystem -> g {_attackDamage = _attackDamage g + amount}
