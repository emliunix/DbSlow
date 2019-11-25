# Dynamic Dispatch

Some stages are combinators, which means they consume some other stages as input. So we need a unified interface for stage, and in tradition, it's volcano style, which is basically an iterator.

So the interface should be like:

```haskell
class Stage where
    stgSchema :: Schema
    stgCursor :: IO Cursor

class Cursor where
    curNext :: StateT Cursor IO (Maybe Row)
    curClose :: StateT Cursor IO ()
```

The code is not what's used (causes problem at implementation stage), but explains.

## Major Problem

The major problem is to embed `StateT Cursor IO` into this generic interface.

My first intention was to use typeclass, however, according to the error message and SO explanation, typeclass is not intended to be used at runtime, instead, it's a compile time (type checking/inferencing) utility. This means when using typeclass, the type context is inferable to the concrete Cursor implementation level. And of course it doesn't satisfy our requirement to combine stages at runtime.

According to Haskell Wiki [Existential Type](https://wiki.haskell.org/Existential_type#Dynamic_dispatch_mechanism_of_OOP), there're ways to do dynamic-dispatch, while typeclass is not an option.

So I picked the first trick and come up with a working interface structure.

## Cursor Impl

```haskell
data Cursor = Cursor
    { curNext :: StateT Cursor IO (Maybe Row)
    , curClose :: StateT Cursor IO ()
    }

class RawCursor c where
    rawCurNext :: StateT c IO (Maybe Row)
    rawCurClose :: StateT c IO ()
    toCursor :: c -> Cursor
    toCursor c = Cursor (_wrap rawCurNext) (_wrap rawCurClose)
        where
            _wrap :: StateT c IO t -> StateT Cursor IO t
            _wrap f = do
                (r, c') <- lift $ runStateT f c
                put $ toCursor c'
                return r
```

## Global IO ?

The proposed interface above enables IO everywhere for cursor implementation. This makes me feel it may not be a good practice. Will try to cut IOs when prototype is finished.

Even these IO something functions look better than plain python effectful functions, the type signature explicitly states what are required to run the code: IO, State Cursor, etc.

I'm also thinking that the toplevel IO strucutre may be necessary since we have to do IO somewhere, if it's not passed in at toplevel, than there's no chance to run IO code.
