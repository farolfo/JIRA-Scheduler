import Data.List(sortBy)

random = 0.5

-- runGA :: [a] -> (a -> Integer) -> (Maybe Integer) -> (Maybe Integer) -> a
runGA randomPopulation fitness mutate crossover eliteCount mutationProb maxIterations acceptedFitnessValue =
  runGAHelper randomPopulation fitness mutate crossover eliteCount mutationProb 1 maxIterations acceptedFitnessValue

-- runGAHelper :: [a] -> (a -> Integer) -> (a -> a) -> Integer -> Float -> Integer -> (Maybe Integer) -> (Maybe Integer) -> a
runGAHelper population fitness mutate crossover eliteCount mutationProb iteration maxIterations acceptedFitnessValue =
  let
    valuedPopulation = (sortBy fitnessComparator) . map (\chromosome -> (chromosome, fitness chromosome)) population
    newPopulation =
      (takeFirst eliteCount) . map (\(x,_) -> x) valuedPopulation ++
      (removeLast eliteCount) . (sortBy fitnessComparator) . map (\_ -> genChild valuedPopulation mutate crossover mutationProb) population
    bestChromosome = head newPopulation
    bestFitness = fitness bestChromosome
  in
    if (runOutOfIterations iteration maxIterations || fitnessIsAccepted bestFitness acceptedFitnessValue) then
      bestChromosome
    else
      runGAHelper newPopulation fitness mutate crossover eliteCount mutationProb (iteration+1) maxIterations acceptedFitnessValue

-- Must order decending
-- fitnessComparator :: ((a,Integer) -> (a,Integer) -> Ordering) -> [(a,Integer)] -> [(a,Integer)]
fitnessComparator (_, v1) (_, v2)
  | v1 > v2 = LT
  | v1 < v2 = GT
  | v1 == v2 = EQ

-- takeFirst :: Integer -> [a] -> [a]
takeFirst 0 _ = []
takeFirst n [] = []
takeFirst n (x:xs) = x:takeFirst (n-1) xs

-- removeLast :: Integer -> [a] -> [a]
removeLast 0 _ = _
removeLast n list =
  if (length list < n) then
    []
  else
    takeFirst (length list - n) list

-- valuedPopulation is a valued and ordered population
-- genChild :: [(a,Integer)] -> (a -> a) -> (a -> a -> a) -> Float -> a
genChild valuedPopulation mutate crossover mutationProb =
  let
    parent1 = getParent valuedPopulation
    parent2 = getParent valuedPopulation
  in
    (mutateHelper mutate mutationProb) . crossover parent1 parent2

-- mutateHelper :: (a -> a) -> Float -> (a -> a)
mutateHelper mutate mutationProb =
  if (random < mutationProb) then
    mutate
  else
    id

-- valuedPopulation is a valued and ordered population
-- getParent :: [(a, Integer)] -> a
getParent valuedPopulation =
  (getParentWithProb random) . addCrossoverProb valuedPopulation

-- addCrossoverProb :: [(a,Integer)] -> [(a,Integer,Float)]
addCrossoverProb valuedPopulation =
  let
    fitSum = sum . map (\(x,y) -> y) valuedPopulation
  in
    map (\(chromosome, fitValue) -> (chromosome, fitValue, fitValue / fitSum)) valuedPopulation

-- getParentWithProb :: Float -> [(a,Integer,Float)] -> a
getParentWithProb prob ((c,_,cProb):xs) =
  if (prob <= cProb) then
    c
  else
    getParentWithProb (prob - cProb) xs

-- runOutOfIterations :: Integer -> (Maybe Integer) -> Bool
runOutOfIterations _ Nothing = False
runOutOfIterations iteration (Just maxIterations) = maxIterations < iteration

-- fitnessIsAccepted :: Integer -> (Maybe Integer) -> Bool
fitnessIsAccepted _ Nothing = False
fitnessIsAccepted value (Just acceptedValue) = value >= acceptedValue
