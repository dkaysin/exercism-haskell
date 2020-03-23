module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

type Roll = Int
type Score = Int

data State = State { nRoll      :: Int
                   , scores     :: [Either BowlingError Score]
                   , frameStack :: [Int]
                   , bonusRolls :: [Int] }
                      deriving(Show)
newState = State { nRoll = 0, scores = [], frameStack = [], bonusRolls = [0] }

-- Due to a very particular hierarchy of error states required by tests, this code is quite messy
score :: [Roll] -> Either BowlingError Int
score xr = sum . take 10 <$> sequence (xs ++ [Left IncompleteGame | length xs < 10])
  where
    res = parse xr newState
    xs = scores res

parse :: [Roll] -> State -> State
parse [] state = state
parse xr state = parse (tail xr) $ step xr state

step :: [Roll] -> State -> State
step [] st = st
step rolls@(r:_) state@State{ nRoll = n, scores = xs, frameStack = fs, bonusRolls = br }
  = state{ nRoll = n+1
         , scores = xs ++ s
         , frameStack = nfs
         , bonusRolls = nbr ++ (subtract 1 <$> br) }
  where
    (s, nfs, nbr)
      | r+sumFrame > 10 || isInvalidRoll r || (length xs >= 10 && all (<=0) br)
          = ([ Left InvalidRoll { rollIndex = n, rollValue = r } ], [], [])
      | (length xs == 9) &&
          ((r == 10 && length rolls < 3)
        || (r+sumFrame == 10 && length rolls < 2))
          = (calcFrame n (fs ++ take 3 rolls) : [ Left IncompleteGame ], [], [])
      | r == 10
          = ([calcFrame n (take 3 rolls)], [], [2])
      | r+sumFrame == 10
          = ([ calcFrame (n-1) (sumFrame : take 2 rolls) ], [], [1])
      | not (null fs)
          = ([ calcFrame (n-1) [sumFrame, r] ], [], [])
      | otherwise = ([], [r], [])
    sumFrame = sum fs

calcFrame :: Int -> [Roll] -> Either BowlingError Score
calcFrame n xr
  = sum <$> mapM (\(r, k) -> if isInvalidRoll r
        then Left InvalidRoll { rollIndex = k, rollValue = r }
        else Right r) (zip xr ((+n) <$> [0..]))

isInvalidRoll :: Int -> Bool
isInvalidRoll r = r < 0 || r > 10
