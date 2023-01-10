module Day22
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

-- Data

type PlayerEffect = Player -> Player

data Turn = PlayerTurn | BossTurn deriving (Show, Eq)

data Effect = Effect {spellEffect :: Spell, effectTimer :: Int} deriving (Show)

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Show, Eq, Enum, Bounded)

data Boss = Boss {bossHP :: Int, bossDamage :: Int} deriving (Show)

data Player = Player {playerHP :: Int, manaGained :: Int, manaSpent :: Int} deriving (Show)

data GameState = GameState {gameTurn :: Turn, getPlayer :: Player, getBoss :: Boss, getEffects :: [Effect]} deriving (Show)

-- Helpers

initialState :: Boss -> GameState
initialState boss = GameState PlayerTurn (Player 50 500 0) boss []

spellMana :: Spell -> Int
spellMana MagicMissile = 53
spellMana Drain = 73
spellMana Shield = 113
spellMana Poison = 173
spellMana Recharge = 229

spellDuration :: Spell -> Int
spellDuration MagicMissile = 0
spellDuration Drain = 0
spellDuration Shield = 6
spellDuration Poison = 6
spellDuration Recharge = 5

damageBoss :: Int -> GameState -> GameState
damageBoss damage state =
  let boss = getBoss state
   in state {getBoss = boss {bossHP = bossHP boss - damage}}

damagePlayer :: Int -> GameState -> GameState
damagePlayer damage state =
  let player = getPlayer state
   in state {getPlayer = player {playerHP = playerHP player - damage}}

healPlayer :: Int -> GameState -> GameState
healPlayer healAmount = damagePlayer (-healAmount)

changeMana :: Int -> GameState -> GameState
changeMana manaAmount state =
  let player = getPlayer state
   in state {getPlayer = player {manaGained = manaGained player + manaAmount}}

applySpellEffect :: Spell -> GameState -> GameState
applySpellEffect MagicMissile = damageBoss 4
applySpellEffect Drain = damageBoss 2 . healPlayer 2
applySpellEffect Poison = damageBoss 3
applySpellEffect Recharge = changeMana 101
applySpellEffect Shield = id

playerArmor :: GameState -> Int
playerArmor state =
  if Shield `elem` map spellEffect (getEffects state) then 7 else 0

applyEffects :: GameState -> GameState
applyEffects state =
  let effects = getEffects state
      state' = foldr (applySpellEffect . spellEffect) state effects
   in state' {getEffects = filter ((> 0) . effectTimer) $ map reduceTimer effects}
  where
    reduceTimer (Effect spell timer) = Effect spell (timer - 1)

castSpell :: Spell -> GameState -> GameState
castSpell spell
  | spellDuration spell > 0 = spendMana . addEffect (Effect spell (spellDuration spell))
  | otherwise = spendMana . applySpellEffect spell
  where
    addEffect e state = state {getEffects = e : getEffects state}
    spendMana state =
      let player = getPlayer state
          mana = spellMana spell
       in state {getPlayer = player {manaSpent = manaSpent player + mana}}

canCastSpell :: GameState -> Spell -> Bool
canCastSpell (GameState _ player _ effects) spell =
  manaGained player - manaSpent player > spellMana spell && spell `notElem` map spellEffect effects

actionPhase :: GameState -> [GameState]
actionPhase state = case gameTurn state of
  PlayerTurn -> map (`castSpell` state) $ filter (canCastSpell state) [minBound ..]
  BossTurn ->
    if bossIsDead state
      then [state]
      else [damagePlayer bossDamageDealt state]
  where
    bossDamageDealt = max 1 (bossDamage (getBoss state) - playerArmor state)

fullTurn :: GameState -> [GameState]
fullTurn = map changeTurn . actionPhase . applyEffects
  where
    changeTurn state = case gameTurn state of
      PlayerTurn -> state {gameTurn = BossTurn}
      BossTurn -> state {gameTurn = PlayerTurn}

possibleOutcome :: PlayerEffect -> Boss -> [GameState]
possibleOutcome playerEffect = performTurns . pure . initialState
    where
        performTurns [] = []
        performTurns states =
          let (finished, pending) = splitGameStates $ concatMap fullTurn states
              in finished ++ performTurns pending

splitGameStates :: [GameState] -> ([GameState], [GameState])
splitGameStates [] = ([], [])
splitGameStates (s : rs) =
  let (finished, pending) = splitGameStates rs
   in if isFinished s then (s : finished, pending) else (finished, s : pending)
  where
    isFinished state = playerIsDead state || bossIsDead state

playerIsDead :: GameState -> Bool
playerIsDead = (<= 0) . playerHP . getPlayer

bossIsDead :: GameState -> Bool
bossIsDead = (<= 0) . bossHP . getBoss

-- Parser

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseHP :: Parser Int
parseHP = string "Hit Points: " *> parseNumber <* newline

parseDamage :: Parser Int
parseDamage = string "Damage: " *> parseNumber <* newline

parseInput :: Parser Boss
parseInput = Boss <$> parseHP <*> parseDamage <* eof

part1 :: Boss -> IO ()
part1 = print . minimum . map (manaSpent . getPlayer) . filter bossIsDead . possibleOutcome id

part2 :: Boss -> IO ()
part2 = print
