# Planner

There're two passes:
1. buildPlanTree
2. buildStageTree

## Plan Tree

It's where we transform SQL AST into a real tree style representation of calculations. It's like a tree of volcanos (stage tree) to be built.

As a side effect, I'm now aware why in LINQ, from statement comes first. The execution order of the SQL statement is:
* From
* Where
* Limit
* Select

## Context

The construction of naming context is vital, (which table bindings, column name resolution rely on). The context is usally layered and follows the execution of stages. For example, in from, we constructs the initial context which pulls in the specified table, where and limit don't affect naming, but they'll use the context for value/type retrieving, then in select, a new naming context is constructed which will be used for outer execution. In the context of sub-select, the new context is therefore the from context used. Or it's the final context and hence the schema of the whole query statement.

The layered nature of context in SQL is kind of limited, the only case I think is for `select *, b as c from`. If say "c" is at layer 1, then other names are resolved at layer 0, and the resolve function of layer 1 should first lookup in self, then proxy to layer 0.

However, since in SQL, usally we need the full schema of a statement, instead of layering context, a full copy and merge seems more nature for `select *, b as c from`.

In procedural language of SQL, maybe it's useful to introduce another layered context implementation.

## Sub-Select

Sub-Select `where c in (select c in t1 where d)` is not supposed to be evaluated directly as a expression. If so it's no different than nested loop join execution. So we need to transform it into a join. Considering the nessecary complexity involved, will make it a separate pass for future.

## Stage

Only merge-join is planned for now. It's a simple merge of two sorted stages. 
