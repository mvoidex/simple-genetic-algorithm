-- | Simple parallel genetic algorithm implementation.
module GA.Simple (
    Chromosome(..),
    runGA,
    runGAIO,
    zeroGeneration,
    nextGeneration
  ) where

import Control.Monad.State
import Control.Parallel.Strategies
import qualified Data.List as L
import System.Random

-- | Chromosome interface
class Chromosome a where
    -- | Crossover function
    crossover :: RandomGen g => a -> a -> State g [a]
    -- | Mutation function
    mutation :: RandomGen g => a -> State g a
    -- | Fitness function. fitness x > fitness y means that x is better than y 
    fitness :: a -> Double

-- | Pure GA implementation
runGA   :: (RandomGen g, NFData a, Chromosome a)
        => g
        -- ^ Random number generator
        -> Int
        -- ^ Population size
        -> Double
        -- ^ Mutation probability [0, 1]
        -> State g a
        -- ^ Random chromosome generator (hint: use currying or closures)
        -> (a -> Int -> Bool)
        -- ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
        -> a
        -- ^ Best chromosome
runGA gen ps mp rnd stopf =
    let (pop, gen') = runState (zeroGeneration rnd ps) gen in
    runGA' gen' pop ps mp stopf 0

runGA' :: (RandomGen g, NFData a, Chromosome a) => g -> [a] -> Int -> Double -> (a -> Int -> Bool) -> Int -> a
runGA' gen pop ps mp stopf gnum =
    let best = head pop in
    if stopf best gnum
        then best
        else
            let (pop', gen') = runState (nextGeneration pop ps mp) gen in
            runGA' gen' pop' ps mp stopf (succ gnum)

-- | Non-pure GA implementation
runGAIO :: (NFData a, Chromosome a)
        => Int
        -- ^ Population size
        -> Double
        -- ^ Mutation probability [0, 1]
        -> State StdGen a
        -- ^ Random chromosome generator (hint: use currying or closures)
        -> (a -> Int -> IO Bool)
        -- ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
        -> IO a
        -- ^ Best chromosome
runGAIO ps mp rnd stopf = do
    gen <- newStdGen
    let (pop, gen') = runState (zeroGeneration rnd ps) gen
    runGAIO' gen' pop ps mp stopf 0

runGAIO' :: (RandomGen g, NFData a, Chromosome a, Monad m) => g -> [a] -> Int -> Double -> (a -> Int -> m Bool) -> Int -> m a
runGAIO' gen pop ps mp stopf gnum = do
    let best = head pop
    stop <- stopf best gnum
    if stop
        then return best
        else do
            let (pop', gen') = runState (nextGeneration pop ps mp) gen
            runGAIO' gen' pop' ps mp stopf (gnum+1)

-- | Generate zero generation. Use this function only if you are going to implement your own runGA.
zeroGeneration  :: (RandomGen g)
                => State g a
                -- ^ Random chromosome generator (hint: use closures)
                -> Int
                -- ^ Population size
                -> State g [a]
                -- ^ Zero generation and new RNG
zeroGeneration rnd ps = replicateM ps rnd

-- | Generate next generation (in parallel) using mutation and crossover.
--   Use this function only if you are going to implement your own runGA.
nextGeneration  :: (NFData a, RandomGen g, Chromosome a)
                => [a]
                -- ^ Current generation
                -> Int
                -- ^ Population size
                -> Double
                -- ^ Mutation probability
                -> State g [a]
                -- ^ Next generation ordered by fitness (best - first) and new RNG
nextGeneration pop ps mp = state $ \gen ->
    let (gen':gens) = L.unfoldr (Just . split) gen
        chunks = L.zip gens $ init $ L.tails pop
        results = map (\(g, (x:ys)) -> [ (t, fitness t) | t <- nextGeneration' [ (x, y) | y <- ys ] g mp [] ]) chunks
                    `using` parList rdeepseq
        lst = take ps $ L.sortBy (\(_, fx) (_, fy) -> fy `compare` fx) $ concat results
    in ( map fst lst, gen' )

nextGeneration' :: (RandomGen g, NFData a, Chromosome a) => [(a, a)] -> g -> Double -> [a] -> [a]
nextGeneration' [] _ _ acc = acc
nextGeneration' ((p1,p2):ps) g0 mp acc =
    let (children0, g1) = runState (crossover p1 p2) g0
        (children1, g2) = L.foldl'
                             (\(xs, g) x -> let (x', g') = runState (mutate x mp) g in (x':xs, g'))
                             ([],g1) children0
    in
    nextGeneration' ps g2 mp (children1 ++ acc)

mutate :: (RandomGen g, Chromosome a) => a -> Double -> State g a
mutate x mp = do
    r <- state $ randomR (0.0, 1.0)
    if r <= mp then mutation x else return x
