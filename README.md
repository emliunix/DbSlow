# DbSlow
Some Database Implementation.

## Reason for this repo

1. to avoid being asked: Do you have basic understanding of SQL/RDBMS? Can you SQL?
2. as a test lab to explore the features I'm interested in:
   * Page Cache System, easily switchable swap algorithm, and how to integrate with different kinds of SQL Loads (scan query, join query, hot range query, etc) / data components (Table, Index, Temporary Data Strucutre like temp table or temp bitmap, etc)
   * Join Algorithms (hash, sort-merge)
   * Query Optimizer
   * Whole Stage Code Generation
   * Windowed/Partitioned computation
   * User-Controllable runtime detail, so we're not stuck with algorithm chosen plans.
   * Concurrency Control
   * OLAP tasks
   
## Concurrency Controll

Basically two kind of open interfaces: Isolation Level / Explicit Locks. There're many interesting topics here:

* SSI
* MVCC
* Hierarchical / Fine-granied locks

## User Interface

Is it possible to provide alternate powerful User Interfaces other than SQL? Or to enrich SQL by more power with syntax extensions. Or provide another complement hint system to alow user control over plan generation and modification.
