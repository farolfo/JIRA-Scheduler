module GA where

import Data.List(sortBy)

data Chromosome a = Chromosome a deriving (Show)

random :: (Fractional a, Ord a) => a
random = 0.5

runGA :: (Fractional a, Ord a) => [(Chromosome t)] -> ((Chromosome t) -> a) -> ((Chromosome t) -> (Chromosome t)) -> ((Chromosome t) -> (Chromosome t) -> (Chromosome t)) -> Int -> a -> (Maybe Int) -> (Maybe a) -> (Chromosome t)
runGA randomPopulation fitness mutate crossover eliteCount mutationProb maxIterations acceptedFitnessValue =
  runGAHelper randomPopulation fitness mutate crossover eliteCount mutationProb 1 maxIterations acceptedFitnessValue

runGAHelper :: (Fractional a, Ord a) => [(Chromosome t)] -> ((Chromosome t) -> a) -> ((Chromosome t) -> (Chromosome t)) -> ((Chromosome t) -> (Chromosome t) -> (Chromosome t)) -> Int -> a -> Int -> (Maybe Int) -> (Maybe a) -> (Chromosome t)
runGAHelper population fitness mutate crossover eliteCount mutationProb iteration maxIterations acceptedFitnessValue =
  let
    valuedPopulation = (sortBy fitnessComparator) ( map (\chromosome -> (chromosome, fitness chromosome)) population )
    newPopulation = ((takeFirst eliteCount) ( map fst valuedPopulation )) ++ map fst ((removeLast eliteCount) ((sortBy fitnessComparator) ( map (\c -> (c, fitness c)) ( map (\_ -> genChild valuedPopulation mutate crossover mutationProb) population ) ) ) )
    bestChromosome = head newPopulation
    bestFitness = fitness bestChromosome
  in
    if (runOutOfIterations iteration maxIterations || fitnessIsAccepted bestFitness acceptedFitnessValue) then
      bestChromosome
    else
      runGAHelper newPopulation fitness mutate crossover eliteCount mutationProb (iteration + 1) maxIterations acceptedFitnessValue

-- Must order decending
fitnessComparator :: (Fractional a, Ord a) => (Chromosome t, a) -> (Chromosome t, a) -> Ordering
fitnessComparator (Chromosome x1, v1) (Chromosome x2, v2)
  | v1 > v2 = LT
  | v1 < v2 = GT
  | v1 == v2 = EQ

takeFirst :: Int -> [a] -> [a]
takeFirst 0 _ = []
takeFirst n [] = []
takeFirst n (x:xs) = x:takeFirst (n-1) xs

removeLast :: Int -> [a] -> [a]
removeLast 0 xs = xs
removeLast n list =
  if (length list < n) then
    []
  else
    takeFirst (length list - n) list

-- valuedPopulation is a valued and ordered population
genChild :: (Fractional a, Ord a) => [((Chromosome t), a)] -> ((Chromosome t) -> (Chromosome t)) -> ((Chromosome t) -> (Chromosome t) -> (Chromosome t)) -> a -> (Chromosome t)
genChild valuedPopulation mutate crossover mutationProb =
  let
    parent1 = getParent valuedPopulation
    parent2 = getParent valuedPopulation
  in
    (mutateHelper mutate mutationProb) ( crossover parent1 parent2 ) -- use dot notation !

mutateHelper :: (Fractional a, Ord a) => ((Chromosome t) -> (Chromosome t)) -> a -> ((Chromosome t) -> (Chromosome t))
mutateHelper mutate mutationProb =
  if (random < mutationProb) then
    mutate
  else
    id

-- valuedPopulation is a valued and ordered population
getParent :: (Fractional a, Ord a) => [((Chromosome t), a)] -> (Chromosome t)
getParent valuedPopulation =
  (getParentWithProb random) ( addCrossoverProb valuedPopulation ) -- use dot notation !!

addCrossoverProb :: (Fractional a, Ord a) => [((Chromosome t), a)] -> [((Chromosome t), a, a)]
addCrossoverProb valuedPopulation =
  let
    fitSum = sum ( map snd valuedPopulation ) -- Use dot notation !!
  in
    map (\(chromosome, fitValue) -> (chromosome, fitValue, fitValue / fitSum)) valuedPopulation

getParentWithProb :: (Fractional a, Ord a) => a -> [((Chromosome t), a, a)] -> (Chromosome t)
getParentWithProb prob ((c,_,cProb):xs) =
  if (prob <= cProb) then
    c
  else
    getParentWithProb (prob - cProb) xs

runOutOfIterations :: Int -> (Maybe Int) -> Bool
runOutOfIterations _ Nothing = False
runOutOfIterations iteration (Just maxIterations) = maxIterations < iteration

fitnessIsAccepted :: (Fractional a, Ord a) => a -> Maybe a -> Bool
fitnessIsAccepted _ Nothing = False
fitnessIsAccepted value (Just acceptedValue) = value >= acceptedValue
