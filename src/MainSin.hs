-- | Example: sin() function interpolation on [0, pi/2]
module Main where

import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Monad.State
import Data.List as L
import System.Random
import Text.Printf

import GA.Simple

newtype SinInt = SinInt [Double]

instance NFData SinInt where
    rnf (SinInt xs) = rnf xs `seq` ()

instance Show SinInt where
    show (SinInt []) = "<empty SinInt>"
    show (SinInt (x:xs)) =
        let start = printf "%.5f" x
            end = concat $ zipWith (\c p -> printf "%+.5f" c ++ "X^" ++ show p) xs [1 :: Int ..]
        in start ++ end

polynomialOrder :: Int
polynomialOrder = 4 :: Int

err :: SinInt -> Double
err (SinInt xs) =
    let f x = snd $ L.foldl' (\(mlt,s) coeff -> (mlt*x, s + coeff*mlt)) (1,0) xs
    in maximum [ abs $ sin x - f x | x <- [0.0,0.001 .. pi/2]]

instance Chromosome SinInt where
    crossover (SinInt xs) (SinInt ys) = return [SinInt (L.map (/ 2) $ L.zipWith (+) xs ys)]
    mutation (SinInt xs) = do
        idx <- state $ randomR (0, length xs - 1)
        dx <- state $ randomR (-10.0, 10.0)
        let
            (h, (t:tl)) = (take idx &&& drop idx) xs
        return $ SinInt $ h ++ [t + t * dx] ++ tl
    fitness int = max_err - min (err int) max_err where
        max_err = 1000.0

randomSinInt :: State StdGen SinInt
randomSinInt = fmap SinInt $ replicateM (succ polynomialOrder) (state $ randomR (-10.0, 10.0))

stopf :: SinInt -> Int -> IO Bool
stopf best gnum = do
    let e = err best
    _ <- printf "Generation: %02d, Error: %.8f\n" gnum e
    return $ e < 0.0002 || gnum > 20

main :: IO ()
main = do
    int <- runGAIO 64 0.1 randomSinInt stopf
    putStrLn ""
    putStrLn $ "Result: " ++ show int
