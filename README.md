JIRA Scheduler
===============

Given a bunch of JIRA tasks for the sprint, the JIRA-Scheduler generates a plan for the next two weeks saying what tasks should be done first given the priority, duration, owner and which tasks block the ones you are doing.

The core for this shceduling problem solver is a genethic algorithm.

This project is written in Haskel and used as a final project for the Functional Programming subject at ITBA.

```haskell
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

-- A backlog, just a bunch of JIRAs
data Backlog = Backlog [Task] deriving (Show);

-- Initialize the JIRA tasks you want to do in the sprint, 
-- set whatever in the 'startHour field', we will fill that for you
task1 = Task {
      identifier="ITBA-1",
      owner="Pablo",
      duration=12,
      blockedBy=[],
      priority=Highest,
      startHour=0
};
task2 = Task {
      identifier="ITBA-2",
      owner="Pablo",
      duration=32,
      blockedBy=[task1],
      priority=Highest,
      startHour=0
};
task1 = Task {
      identifier="ITBA-3",
      owner="Juan",
      duration=6,
      blockedBy=[task1,task2],
      priority=Highest,
      startHour=0
};

-- Put the JIRAs in the backlog
backlog = Backlog [task1, task2, task3];

-- Execute the JIRA scheduler and the tasks will have now the start hour property set as it should be
-- JIRAScheduler :: Backlog -> Backlog
scheduledBacklog = JIRAScheduler backlog;
```

###Some observations

* It is assumed that the attributes are always given in the Tasks, i.e.: no empty owners, nor duration.
* It is assumed we have NO cycles betwen the JIRAs 'blocked by' chain, i.e.: no A blocked by B and B blocked by A, directly or not.

