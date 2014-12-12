-- A Task properties
data Id = Id String;                                    -- The identifier of the JIRA task
data Owner = Owner String;                              -- The owner of the JIRA
data Duration = Duration Integer;                       -- The duration in hours that the task will require to be developed
data BlockedBy = BlockedBy [Task];                      -- All the tasks that are blocking the development of this one
data Priority = Highest | High | Medium | Low | Lowest; -- The priority of this task
data StartHour = StartHour Integer;                     -- Hour from the begining of the sprint when the task should start

-- A JIRA task
data Task = Task Id Owner Duration BlockedBy Priority StartHour;

-- A backlog, jsut a bunch of JIRAs
data Backlog = Backlog [Task];

instance Show Priority where
      show Highest = show "Highest"
      show High = show "High"
      show Medium = show "Medium"
      show Low = show "Low"
      show Lowest = show "Lowest"

instance Show BlockedBy where
      show (BlockedBy []) = show ""
      show (BlockedBy ((Task (Id id) _ _ _ _ _):xs)) = show id ++ "," ++ show (BlockedBy xs)

instance Show Task where
      show (Task (Id id) (Owner o) (Duration d) blockedBy prior (StartHour hour)) = 
            show ("{id:" ++ show id ++ ",owner:" ++ show o ++ ",duration:" ++ show d ++ ",blockedBy:[" ++ show blockedBy ++ "],startHour:" ++ show hour ++ "}"); 

instance Show Backlog where
      show (Backlog []) = show ""
      show (Backlog (task:xs)) = show task ++ "," ++ show (Backlog xs) 

-- Initialize the JIRA tasks you want to do in the sprint
task1 = Task 
            (Id "ITBA-1")
            (Owner "Pablo")
            (Duration 12)
            (BlockedBy [])
            (Highest)
            (StartHour 0);
task2 = Task 
            (Id "ITBA-2")
            (Owner "Pablo")
            (Duration 3)
            (BlockedBy [task1])
            (Highest)
            (StartHour 0);
task3 = Task 
            (Id "ITBA-3")
            (Owner "Juan")
            (Duration 6)
            (BlockedBy [task1,task2])
            (Highest)
            (StartHour 0);

-- randomizeTask :: Task -> Task
randomizeTask (Task (Id id) (Owner o) (Duration d) (BlockedBy []) p _) = Task (Id id) (Owner o) (Duration d) (BlockedBy []) p (StartHour 0);
randomizeTask (Task (Id id) (Owner o) (Duration d) (BlockedBy (x:xs)) p _) = Task (Id id) (Owner o) (Duration d) (BlockedBy (x:xs)) p (StartHour 12); -- this should be randomHour

-- genRandomTasks :: Backlog -> Backlog;
genRandomTasks (Backlog tasks) = Backlog (map randomizeTask tasks);

ans = genRandomTasks (Backlog [task1, task2, task3])


main = do  
      print "JIRA Scheduler v1.0"
      print ans 
--JIRAScheduler backlog = runGA (genRandomTasks backlog) taskFitness crossTasks;

-- Execute the JIRA scheduler and the tasks will have now the start hour property set as it should be
-- JIRAScheduler :: Backlog -> Backlog
--scheduledBacklog = JIRAScheduler (Backlog [task1, task2, task3]);
