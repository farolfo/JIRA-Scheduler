JIRA Scheduler
===============

Given a bunch of JIRA tasks for the sprint, the JIRA-Scheduler generates a plan for the next two weeks saying what tasks should be done first given the priority, duration, owner and which tasks block the ones you are doing.

The algorithm to be used for scheduleing the tasks will be simulated anealing or genetic algorithms, TBD.

This project is being done in Haskel to be used as a final project for the Functional Programming subject at ITBA.


```haskell
-- A Task properties
data Owner = Owner String;                              -- The owner of the JIRA
data Duration = Duration Integer;                       -- The duration in hours that the task will require to go 
                                                         -- from OPEN to DONE.
data BlockedBy = BlockedBy [Task];                      -- All the tasks that are blocking the development of this one.
data Priority = Highest | High | Medium | Low | Lowest; -- The priority of this task.
data State = Open | Done;                               -- The state of the task. (Not taking into account QA phase)

-- A JIRA task
data Task = Task Owner Duration BlockedBy Priority State;

-- A sprint day. A snapshot of how the tasks should be at the end of the day.
data SprintDay = SprintDay [Task];

-- A sprint plan. The schedule with a snapshot of how each task should be at the end of each day.
data SprintPlan = SprintPlan [SprintDay];

-- Initialize the JIRA tasks you want to do in the sprint.
task1 = Task 
            (Owner "Pablo")
            (Duration 12)
            (BlockedBy [])
            (Highest)
            (Open);
task2 = Task 
            (Owner "Pablo")
            (Duration 3)
            (BlockedBy [task1])
            (Highest)
            (Open);
task3 = Task 
            (Owner "Juan")
            (Duration 6)
            (BlockedBy [task1,task2])
            (Highest)
            (Open);

-- Put the JIRAs in an initial sprint day
sprintDay0 = SprintDay [task1, task2, task3];

-- Execute the JIRA scheduler and you will have your sprint plan
-- JIRAScheduler :: SprintDay -> SprintPlan
sprintPlan = JIRAScheduler sprintDay0;
```

###Some observations

* It is assumed that the attributes are always given in the Tasks, i.e.: no empty owners, nor duration.
* It is assumed we have NO cycles betwen the JIRAs 'blocked by' chain, i.e.: no A blocked by B and B blocked by A, directly or not.

