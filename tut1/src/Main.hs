import Control.Monad.Random

data SchoolState = Facebook | ClassOne | ClassTwo | ClassThree
                 | Pub | Pass | Sleep

allStates :: [SchoolState]
allStates = [Facebook, ClassOne, ClassTwo, ClassThree,
              Pub, Pass, Sleep]

gamma :: Double
gamma = 0.5

instance Show SchoolState where
  show Facebook = "Facebook"
  show ClassOne = "ClassOne"
  show ClassTwo = "ClassTwo"
  show ClassThree = "ClassThree"
  show Pub = "Pub"
  show Pass = "Pass"
  show Sleep = "Sleep"

reward :: SchoolState -> Rational
reward Facebook = -1
reward ClassOne = -2
reward ClassTwo = -2
reward ClassThree = -2
reward Pub = 1
reward Pass = 10
reward Sleep = 0

transitionProbability :: SchoolState -> SchoolState -> Rational
transitionProbability Facebook Facebook = 0.9
transitionProbability Facebook ClassOne = 0.1

transitionProbability ClassOne Facebook = 0.5
transitionProbability ClassOne ClassTwo = 0.5

transitionProbability ClassTwo Sleep = 0.2
transitionProbability ClassTwo ClassThree = 0.8

transitionProbability ClassThree Pass = 0.6
transitionProbability ClassThree Pub = 0.4

transitionProbability Pub ClassOne = 0.2
transitionProbability Pub ClassTwo = 0.4
transitionProbability Pub ClassThree = 0.4

transitionProbability Pass Sleep = 1
transitionProbability Pass _ = 0

transitionProbability _ _ = 0


getPossibleNextStates :: SchoolState -> [(SchoolState, Rational)]
getPossibleNextStates thisState =
  filter (( > 0) . snd) $ [ (s, transitionProbability thisState s) | s <- allStates]


expectedReward :: SchoolState -> Rational
expectedReward s = sum $ map (\(x, p) -> p * reward x) $ getPossibleNextStates s


nextState :: SchoolState -> IO (SchoolState)
nextState s = fromList $ getPossibleNextStates s

trace :: SchoolState -> IO [(SchoolState, Rational)]
trace Sleep = do
  -- print Sleep
  return [(Sleep, 0)]
trace s = do
  next <- nextState s
  -- print s
  restOfTrace <- trace next
  return $ (s, reward s):restOfTrace

value :: SchoolState -> IO Double
value s = do
  states <- trace s
  return $ sum $ zipWith (*) [fromRational v | (_, v) <- states] gammas
    where
      gammas = zipWith (^) (repeat gamma) [0..]

averageValue :: Int -> SchoolState -> IO Double
averageValue n s = do
  values <- sequence [value s | _ <- [1..n]]
  return $ (sum values) / (fromIntegral n)


main :: IO ()
main = do
  values <- fmap (zip allStates) $ sequence $ map (averageValue 5000) $ allStates
  print $ show values
  return ()
