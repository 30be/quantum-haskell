{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Exception (assert)
import Control.Monad (replicateM_)
import Data.Complex (Complex (..), conjugate, magnitude)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import System.Random (randomRIO)

type CMPL = Complex Double

data Matrix' a = Matrix {w :: Int, h :: Int, getVector :: V.Vector a} deriving (Eq, Show, Functor)

matrix :: [CMPL] -> Matrix
matrix a = assert (n * n == length a) $ Matrix n n (V.fromListN (n * n) a)
  where
    n = floor $ sqrt $ fromIntegral $ length a

unitMatrix :: Int -> Matrix
unitMatrix n = Matrix n n $ V.generate (n * n) $ \i -> if i `div` n == i `mod` n then 1 else 0

type Matrix = Matrix' CMPL

type QState = V.Vector CMPL

infixl 7 *^

(*^) :: Matrix -> QState -> QState
(Matrix w h m) *^ v = V.generate h $ \row -> V.sum $ V.zipWith (*) v (V.slice (row * w) w m)

matrixMul :: Matrix -> Matrix -> Matrix
matrixMul (Matrix w1 h1 v1) (Matrix w2 h2 v2)
  | w1 /= h2 = error "Dimensions dont match!"
  | otherwise = Matrix w2 h1 $ V.generate (h1 * w2) $ \i ->
      let (r, c) = i `divMod` w2
       in sum [v1 V.! (r * w1 + k) * v2 V.! (k * w2 + c) | k <- [0 .. w1 - 1]]

matrixSub :: Matrix -> Matrix -> Matrix
matrixSub (Matrix w1 h1 v1) (Matrix w2 h2 v2)
  | w1 /= w2 || h1 /= h2 = error "Dimensions dont match"
  | otherwise = Matrix w1 h1 $ V.zipWith (-) v1 v2

q0, q1 :: QState
q0 = [1, 0]
q1 = [0, 1]

closeTo :: QState -> QState -> Bool
closeTo a b = all (\(a, b) -> magnitude (a - b) < 1e-10) $ V.zip a b

shouldBe :: QState -> QState -> IO ()
shouldBe a b
  | a `closeTo` b = pure ()
  | otherwise = error $ show a <> " != " <> show b <> "\n"

xGate, yGate, zGate, hadamard :: Matrix
xGate = matrix [0, 1, 1, 0]
yGate = matrix [0, 0 :+ (-1), 0 :+ 1, 0]
zGate = matrix [1, 0, 0, -1]
hadamard = (/ sqrt 2) <$> matrix [1, 1, 1, -1]

transpose :: Matrix -> Matrix
transpose (Matrix w h v) = Matrix h w $ V.fromListN (w * h) [v V.! (y * w + x) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-- Not used
adjoint :: Matrix -> Matrix
adjoint = transpose . fmap conjugate

-- oracle|x> = (-1)^f(x)|x>
-- oracle is 2^N * 2^N matrix
-- Result is an integer x in the range [0..2^N-1] such that its qubit representation q |00101> is of size N, and f(x) == 1
grover :: Matrix -> QState
grover oracle@(Matrix n _ _) = iterate groverIteration s !! floor (pi / 4 * sqrt (fromIntegral n))
  where
    -- oracle flips around the answer-axis
    -- diffusion flips around the s-axis
    -- for an explanation on why does this bring the quantum state closer to the desired answer, see https://youtu.be/RQWpF2Gb-gU?t=1524
    groverIteration psi = diffusion `matrixMul` oracle *^ psi
    s = V.replicate n $ 1 / sqrt (fromIntegral n) -- uniform superposition |s> = 1/sqrt(n) * sum_x |x>
    diffusion = proj `matrixSub` unitMatrix n -- 2|s><s| - I - it is a reflection around  the s - the Householder transformation
    proj = fmap (* 2) $ Matrix n n $ (*) <$> s <*> s

measure :: QState -> IO Int
measure v = do
  let probabilities = (^ 2) . magnitude <$> v
  r <- randomRIO (0.0, V.sum probabilities)
  return $ fromMaybe (V.length probabilities - 1) $ V.findIndex (> r) (V.scanl1 (+) probabilities)

-- \| This may be much more complex, like calculating whether the hash matches - but i dont have any desire to calculate a hash match on logic gates as that would require manually compiling a complicated program to gates
verify :: Int -> Bool
verify = (== 12)

-- | Quantum-compiled version of the check
-- | 16x16 matrix, such that when multiplied by vector [a,b,...l,...p] - 16 numbers overall, returns
-- returns vector [a,b,...-l,...p], i.e. flips the magnitude of the 12th element.
-- In general, the index of the element being flipped is *not known*
--
-- The vector corresponds to the magnitudes of the superposited quantum states
--
-- |0001> * a
-- |0010> * b
-- ...
-- |1010> * l
-- ...
-- |1111> * p
--
-- This matrix is a classical-computer representation of a set of quantum logical gates
--
-- Also, this can be seen in the form of a Householder reflection: U_w = I - 2 |w> <w|
verifyQ :: Matrix
verifyQ = Matrix w h $ m V.// [(12 * w + 12, -1)]
  where
    (Matrix w h m) = unitMatrix 16

groverRepeat :: Matrix -> IO Int
groverRepeat oracle = do
  x <- measure $ grover oracle
  if verify x
    then pure x
    else putStrLn "Did not hit 12!" >> groverRepeat oracle -- Missing happens from time to time

test :: IO ()
test = do
  (xGate *^ q0) `shouldBe` q1 -- Not functional, just playing around
  (xGate *^ q1) `shouldBe` q0
  putStrLn "And the answer is:"
  replicateM_ 100 $ print =<< groverRepeat verifyQ

-- (hadamard *^ q0) `shouldBe` ((q0 +. q1) /. sqrt 2)
-- (hadamard *^ q1) `shouldBe` ((q0 -. q1) /. sqrt 2)

main :: IO ()
main = test >> putStrLn "Hello, Haskell!"
