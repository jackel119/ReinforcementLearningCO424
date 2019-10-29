{-# LANGUAGE PatternSynonyms, Strict #-}

module Main where

import Control.Monad.Random
import Control.Monad.State
import Data.Function
import qualified Data.List as L
import qualified Data.List.Extras.Argmax as Argmax
import qualified Data.Map.Strict as Map
import Data.Maybe

p :: Double
p = 0.8

gamma :: Double
gamma = 0.8

-- Data type for each square in the Grid
-- row, column coordinates of a grid square
-- For example:
--  GridState 1 1 represents top left square (s1)
--  GridState 2 4 represents  (s7)
--  See Show instance for full list
data GridState = GridState Int Int
  deriving (Eq, Ord)

-- Makes debugging/printing correspond
-- to actual state numbers
instance Show GridState where
  show (GridState 1 1) = "s1"
  show (GridState 1 2) = "s2"
  show (GridState 1 3) = "s3"
  show (GridState 1 4) = "s4"
  show (GridState 2 1) = "s5"
  show (GridState 2 2) = "s6"
  show (GridState 2 4) = "s7"
  show (GridState 3 2) = "s8"
  show (GridState 3 3) = "s9"
  show (GridState 3 4) = "s10"
  show (GridState 4 3) = "s11"


-- Convenient synonyms between
-- states s1...11 and actual code implementation
s1  = GridState 1 1
s2  = GridState 1 2
s3  = GridState 1 3
s4  = GridState 1 4
s5  = GridState 2 1
s6  = GridState 2 2
s7  = GridState 2 4
s8  = GridState 3 2
s9  = GridState 3 3
s10 = GridState 3 4
s11 = GridState 4 3

-- Pattern Synonyms
pattern RewardState  <- GridState 1 3
pattern PenaltyState <- GridState 4 3

-- Data type for possible actions
data Action = North | East | South | West
  deriving (Show, Eq)

-- Data Type for Policy
-- Map from state -> action
type Policy = Map.Map GridState Action

-- Data Type for Value function
-- Map state -> value
type Values = Map.Map GridState Double

type S a = State (Values, Policy) a

-- List of all valid states
allStates :: [GridState]
allStates = [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11]

-- Function to validate states
-- e.g. GridState -1 -1 isn't a valid state
validState :: GridState -> Bool
validState = flip elem allStates

-- List of all actions
-- For convenience, given that all actions
-- are possible in all states
allActions :: [Action]
allActions = [North, East, South, West]

-- Given a current state and a policy,
-- return the action to take
getAction :: GridState -> Policy -> Action
getAction g p = fromJust $ Map.lookup g p

getValue :: GridState -> Values -> Double
getValue g vs = fromJust $ Map.lookup g vs

-- Generates an initial random policy
initRandomPolicy :: IO Policy
initRandomPolicy
  = Map.fromAscList <$> stateToActions
  where
    stateToActions = mapM (\s -> sequence (s, randomChoice allActions)) allStates

initSingularPolicy :: Action -> Policy
initSingularPolicy a = Map.fromAscList [(s, a) | s <- allStates]

-- Initialize values of all states to 0
initZeroValues :: Values
initZeroValues = Map.fromAscList $ zip allStates $ repeat 0

initRandomValues :: IO Values
initRandomValues = Map.fromAscList <$> stateValuePairs
  where
    stateValuePairs :: IO [(GridState, Double)]
    stateValuePairs
      = sequence [ sequence (s, randomRIO (-20, 20)) | s <- allStates ]

-- Initialize values of all states consecutively
initSequentialValues :: Values
initSequentialValues = Map.fromAscList $ zip allStates [1..]

initState :: IO (Values, Policy)
initState = do
  v <- initRandomValues
  p <- initRandomPolicy
  return (v, p)

-- Given an action that we chose,
-- determine whether that is the actual action taken
-- or the other directions are chosen instead
determineAction :: Action -> IO Action
determineAction a = do
  x <- randomIO :: IO Double
  if x <= p
     then return a
     else randomChoice otherMoves
  where
    otherMoves = L.delete a allActions

randomChoice :: [a] -> IO a
randomChoice xs = do
  randomIndex <- randomRIO (0, length xs - 1)
  return $ xs !! randomIndex


-- Given a state and a DETERMINED move,
-- return the resulting state.
move :: GridState -> Action-> GridState
move oldState action = if validState newState then newState else oldState
  where
    move' :: GridState -> Action-> GridState
    move' (GridState row col) North = GridState (row - 1) col
    move' (GridState row col) South = GridState (row + 1) col
    move' (GridState row col) East = GridState row (col + 1)
    move' (GridState row col) West = GridState row (col - 1)
    newState = move' oldState action

-- Reward Function.
-- Given that reward function in this specific instance
-- is determined solely by the state we are transitioning to,
-- we can make it a function that only takes in the next state
reward :: GridState -> Double
reward RewardState  = 10   -- Personalized reward state = s3
reward PenaltyState = -100 -- Penalty State s11
reward _ = -1

-- Given that our deterministic policy has chosen a move,
-- calculate the new value of that particular state.
-- Sum of P[R + gamma * v], defined in slide 139
expectedReturn :: Values -> GridState -> Action -> Double
expectedReturn vs currentSquare action
  = sum $! zipWith (*) probabilities r
  where
    probabilityOtherMove = (1 - p) / 3
    probabilities = p : replicate 3 probabilityOtherMove
    otherActions = action : L.delete action allActions
    nextStates = map (move currentSquare) otherActions
    immediateRewards = map reward nextStates
    futureRewards = map (* gamma) $ map (flip getValue vs) nextStates
    r = zipWith (+) futureRewards immediateRewards

argMaxAction :: GridState -> Values -> Action
argMaxAction g vs = Argmax.argmax (expectedReturn vs g) allActions

-- Function for Policy Iteration.
policyIteration :: S ()
policyIteration = do
  policyEvaluation
  improved <- policyImprovement
  if improved
     then policyIteration
     else return ()

-- Policy Improvement (Updates Policy)
-- If Policy does not improve, then
policyImprovement :: S Bool
policyImprovement = state pI
  where
    pI :: (Values, Policy) -> (Bool, (Values, Policy))
    pI (v, oldPolicy) = (policyChanged, (v, newPolicy))
      where
        newPolicy = Map.mapWithKey (\s _ -> argMaxAction s v) oldPolicy
        policyChanged = newPolicy /= oldPolicy

-- Policy Evaluation (Updates Values)
policyEvaluation :: S ()
policyEvaluation = state $ \(v, p) -> ((), (policyEvaluateIteration v p, p))
  where
    -- Helper function for iterating policy evaluation
    policyEvaluateIteration :: Values -> Policy -> Values
    policyEvaluateIteration vs p
      = if maxDifference < 0.000001
           then vs
           else policyEvaluateIteration newValues p
      where
      newValues
        = Map.mapWithKey (\s _ -> expectedReturn vs s (getAction s p)) vs
      valueDifferences = Map.elems $!  Map.unionWith (\x1 x2 -> abs $! x1 - x2) vs newValues
      maxDifference = maximum valueDifferences

printResults :: (Values, Policy) -> IO ()
printResults (v, p) = do
  mapM_ (\s -> putStrLn $
        (show s) ++ ": "
        ++ (show $ getAction s p) ++ "  "
        ++ (show $ getValue s v)) allStates

main :: IO ()
main = do
  initial <- initState
  let (_, finalState) = runState policyIteration initial
  printResults finalState
