JIRA Scheduler
===============

Given a bunch of JIRA tasks for the sprint, the JIRA-Scheduler generates a plan for the next two weeks saying what tasks should be done first given the priority, duration, owner and which tasks block the ones you are doing.

The algorithm to be used for scheduleing the tasks will be simulated anealing or genetic algorithms, TBD.

This project is being done in Haskel to be used as a final project for the Functional Programming subject at ITBA.


```haskell
-- A Task properties
data Owner = Owner String;                              -- The owner of the JIRA
data Duration = Duration Integer;                       -- The duration in hours that the task will require to be developed
data BlockedBy = BlockedBy [Task];                      -- All the tasks that are blocking the development of this one
data Priority = Highest | High | Medium | Low | Lowest; -- The priority of this task
data StartHour = StartHour Integer;                               -- Hour from the begining of the sprint when the task should start

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

-- Put the JIRAs in the backlog
backlog = Backlog [task1, task2, task3];

-- Execute the JIRA scheduler and the tasks will have now the start hour property set as it should be
-- JIRAScheduler :: Backlog -> Backlog
scheduledBacklog = JIRAScheduler backlog;
```

###Some observations

* It is assumed that the attributes are always given in the Tasks, i.e.: no empty owners, nor duration.
* It is assumed we have NO cycles betwen the JIRAs 'blocked by' chain, i.e.: no A blocked by B and B blocked by A, directly or not.

