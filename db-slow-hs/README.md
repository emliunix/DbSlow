# db-slow-hs

## Planed Features

* Stages
  * projection
  * filter
  * join (MergeSort)
  * tableScan
  * sort
* data
  * schema inference
  * schema/assets manage
* more types
  * int/bool/string
  * nullable
  * type annotation
    * expression
    * table columns
    * table-like structure
* type checking
* syntax surgar
  * IN / Sub-Select as Joins
* planner
  * plan generation
  * equivalence tranx rules
  * scoring


## MergeSort

I'm considering implementing it on top of some primitive stages:

MergeSort:
* merge merge-condition
  * sort stage1 sort-key sort-dir
  * sort stage2 sort-key sort-dir
