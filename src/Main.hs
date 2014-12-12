main = print "JIRA Scheduler v1.0"

-- A Task properties
data Owner = Owner String;                              -- The owner of the JIRA
data Duration = Duration Integer;                       -- The duration in hours that the task will require to be developed
data BlockedBy = BlockedBy [Task];                      -- All the tasks that are blocking the development of this one
data Priority = Highest | High | Medium | Low | Lowest; -- The priority of this task
data StartHour = StartHour Integer;						-- Hour from the begining of the sprint when the task should start

-- A JIRA task
data Task = Task Owner Duration BlockedBy Priority StartHour;

-- A backlog, jsut a bunch of JIRAs
data Backlog = Backlog [Task];

-- Initialize the JIRA tasks you want to do in the sprint
task1 = Task 
            (Owner "Pablo")
            (Duration 12)
            (BlockedBy [])
            (Highest)
            (StartHour 0);
task2 = Task 
            (Owner "Pablo")
            (Duration 3)
            (BlockedBy [task1])
            (Highest)
            (StartHour 0);
task3 = Task 
            (Owner "Juan")
            (Duration 6)
            (BlockedBy [task1,task2])
            (Highest)
            (StartHour 0);

randomizeTask :: Task -> Task
randomizeTask (Task (Owner o) (Duration d) (BlockedBy []) (Priority p) _) = Task (Owner o) (Duration d) (BlockedBy []) (Priority p) (StartHour 0);
randomizeTask (Task (Owner o) (Duration d) (BlockedBy (x:xs)) (Priority p) _) = Task (Owner o) (Duration d) (BlockedBy (x:xs)) (Priority p) (StartHour randomStartHour);

genRandomTasks :: Backlog -> Backlog;
genRandomTasks (Backlog tasks) = Backlog (map randomizeTask tasks);


JIRAScheduler backlog = runGA (genRandomTasks backlog) taskFitness crossTasks;

-- Execute the JIRA scheduler and the tasks will have now the start hour property set as it should be
-- JIRAScheduler :: Backlog -> Backlog
scheduledBacklog = JIRAScheduler (Backlog [task1, task2, task3]);

