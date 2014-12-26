data Priority = Highest | High | Medium | Low | Lowest deriving (Show); 

-- A JIRA task
data Task = Task {
      id :: String,           -- The identifier of the JIRA task
      owner :: String,        -- The owner of the JIRA
      duration :: Integer,    -- The duration in hours that the task will require to be developed
      blockedBy :: [Task],    -- All the tasks that are blocking the development of this one
      priority :: Priority,   -- The priority of this task
      startHour :: Integer    -- Hour from the begining of the sprint when the task should start
} deriving (Show);

-- A backlog, jsut a bunch of JIRAs
data Backlog = Backlog [Task] deriving (Show);

-- The chromosome, a proposed solution of our problem
-- data Chromosome a = Chromosome a;

-- The crossover combines two different chromosomes (solutions) into one new
-- data Crossover = Crossover (A -> A -> A)

-- The population, just a bunch of solutions

-- GA Configuration
-- data GA = GA Population Fitness Crossover Mutation CrossoverProb MutationProb EliteSelection;    

instance Eq Task where
   (Task id1 _ _ _ _ _ ) == (Task id2 _ _ _ _ _ ) = (id1 == id2)
   (Task id1 _ _ _ _ _ ) /= (Task id2 _ _ _ _ _ ) = (id1 /= id2)

-- getOwner :: Task -> String
getOwner (Task _ o _ _ _ _) = o;

-- getId :: Task -> String
getId (Task id _ _ _ _ _) = id;

-- overlapTime :: Task -> Task -> Bool
overlapTime (Task _ _ d1 _ _ sh1) (Task _ _ d2 _ _ sh2) = (sh2 < (sh1+d1)) && (sh1 < sh2+d2);

-- randomizeTask :: Task -> Task
randomizeTask (Task id o d [] p _) = Task id o d [] p 0;
randomizeTask (Task id o d (x:xs) p _) = Task id o d (x:xs) p 0; -- this should be randomHour

-- genRandomTasks :: Backlog -> Backlog;
genRandomBacklog (Backlog tasks) = Backlog (map randomizeTask tasks);

-- merge :: [a] -> [a] -> [a]; -- Merges the list mixing between odds and even elements from each list
merge [] l = l;
merge l [] = l;
merge (x:xs) (y:yx) = x : merge yx xs; 

-- crossBacklogs :: Backlog -> Backlog -> Backlog
crossBacklogs (Backlog tasks1) (Backlog tasks2) = merge tasks1 tasks2;

-- hasOverlappedTasks :: [Task] -> Bool
hasOverlappedTasks [] = False;
hasOverlappedTasks tasks = [ (y,x) | y <- tasks, x <- tasks, (getId x) /= (getId y), (getOwner x) == (getOwner y), (overlapTime x y)] /= [];

-- checkStartHour :: [Tasks] -> Integer -> Bool
checkStartHour [] _ = True;
checkStartHour ((Task _ _ d _ _ sh):xs) time = if ( time < (sh + d) ) then False else (checkStartHour xs time);

-- respectsBlockingTasks :: [Task] -> Bool
respectsBlockingTasks [] = True;
respectsBlockingTasks (task:tasksLeft) = if ( respectsBlockingTasksHelper task ) then (respectsBlockingTasks tasksLeft) else False;

respectsBlockingTasksHelper (Task _ _ _ [] _ _) = True;
respectsBlockingTasksHelper (Task _ _ _ tasks _ sh) = checkStartHour tasks sh;

-- backlogFitness :: Backlog -> Integer
backlogFitness (Backlog tasks) = 
      if ( hasOverlappedTasks tasks ) 
            then 0
            else if ( not ( respectsBlockingTasks tasks ) )
                  then 0
                  else 1;

-- Initialize the JIRA tasks you want to do in the sprint
task1 = Task "ITBA-1"
            "Pablo"
            12
            []
            Highest
            0;
task2 = Task "ITBA-2"
            "Pablo"
            32
            [task1]
            Highest
            13;
task3 = Task "ITBA-3"
            "Juan"
            6
            [task1,task2]
            Highest
            22;
taskOverlapped = Task "ITBA-123"
            "Juan"
            6
            [task1,task2]
            Highest
            6;
taskOverlappedPablo = Task "ITBA-123"
            "Pablo"
            6
            [task1,task2]
            Highest
            6;
 task4 = Task "ITBA-3"
            "Juan"
            6
            [task1,task2]
            Highest
            46;           

-- assert :: Bool -> String -> String
assert test testName = if (test) then testName ++ ": OK" else testName ++ ": FAIL"

main = do  
      print "JIRA Scheduler v1.0"
      print "Running tests"
      print (assert (not ( overlapTime task1 task2 )) "OverlapTimeTest1")
      print (assert (overlapTime task1 taskOverlapped) "OverlapTimeTest2")
      print (assert (not (hasOverlappedTasks [task1,task2])) "HasOverlappedTasksTest1")
      print (assert (not (hasOverlappedTasks [task1,taskOverlapped])) "HasOverlappedTasksTest2")
      print (assert (hasOverlappedTasks [task1,taskOverlappedPablo]) "HasOverlappedTasksTest3")
      print (assert (respectsBlockingTasks [task1,task2]) "RespectsBlockingTasksTest1")
      print (assert (not (respectsBlockingTasks [task1,task2,task3])) "RespectsBlockingTasksTest2")
      print (assert (respectsBlockingTasks [task1,task2,task4]) "RespectsBlockingTasksTest3")


