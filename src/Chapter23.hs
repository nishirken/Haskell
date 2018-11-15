module Chapter23 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import System.Random
import Control.Monad.Trans.State (state, evalState, State)

data Die =
  DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  -- Use this tactic _extremely_ sparingly.
  x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieFourTimes :: (Die, Die, Die, Die)
rollDieFourTimes = do
  -- this will produce the same results every
  -- time because it is free of effects.
  -- This is fine for this demonstration.
  (intToDie d1, intToDie d2, intToDie d3, intToDie d4)
    where
      s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, s3) = randomR (1, 6) s2
      (d4, _) = randomR (1, 6) s3

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie
