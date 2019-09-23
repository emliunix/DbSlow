# For Storage, pickle files to store list of mono-type objects
# Language to support
# * create
# * select where
# * (maybe) select from a full outer join b
# * insert into tbl () values ()
# * delete from tbl where
# Index (Oh! in-memory btree_map<k, list_idx>)
# Then we got query planner
# 
# Off-Topic, I'm considering how to represent the queries, maybe in some IR
# so far, It seems query planner involves:
# * select features to support (simple select where, join, subquery, subquery with filter)
#
