# Implicit Cast

It's common to do implicit casts for some primitive types of values, and even further, Scala uses a implicit system to make these implicit behaviours more customizable.
And I'm thinking about the cast systems used in this SQL executor project, both implicit and explicit.

For a specific case, there're many cast candidates, for example. Int -> Long -> Long Long, Int -> Float -> Double, and some explicit ones, Long -> Int, Double -> Int, Float -> Int. And these candidates do have priorities, we don't want Int + Float to be casted into Double + Double.

So ideally, for + op that comes with two arguments, all the cast candidates will be (Castable from int, int) x (Castable from float, float). Give the cartesian product nature, we may want to employ breadth first search strategy, and also, for a given ith argument of op/fn, the acceptable types is known at searching, so we can intersect it with the castable list first. And if after intersection, any of the list is empty, then there's no candidate at all, we can terminate early.

The findCast algorithm:
```haskell
-- not implemented yet
```