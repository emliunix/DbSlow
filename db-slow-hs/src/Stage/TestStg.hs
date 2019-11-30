module Stage.TestStg
    ( mkTestStg
    ) where

import Control.Monad.State.Lazy

import Stage.Def
    ( Stage(..)
    , Schema(..)
    , Cursor
    , RawCursor (..)
    )
import Def (SqlColumn (..), SqlType (..))

-- # Test Cursor #

mkTestStg :: Int -> Stage
mkTestStg initVal = Stage
    { stgSchema = Schema
                    { schCols = [SqlColumn { sColName = "test", sColType = STInt }]
                    , schName = "testStg"
                    }
    , stgNewCursor = return $ toCursor TestCursor { testIdx = initVal }
    }

data TestCursor = TestCursor { testIdx :: Int }

instance RawCursor TestCursor where
    rawCurNext = do
        x <- get
        put $ x { testIdx = 1 + (testIdx x) }
        return $ Just $ [show $ testIdx x]
    rawCurClose = return ()
