module Stage.TestDataStage
    ( mkTestDataStage
    ) where

import Control.Monad.State.Lazy

-- import Stage.Def
--     ( Stage(..)
--     , Schema(..)
--     , Cursor
--     , RawCursor (..)
--     )
-- import Def (SqlColumn (..), SqlType (..))

-- -- # Test Cursor #

import Def

data TestDataCursor = TestDataCursor
    { testDCurData :: [Row]
    }

instance RawCursor TestDataCursor where
    -- rawCurNext :: StateT c IO (Maybe Row)
    rawCurNext = do
        cur <- get
        case testDCurData cur of
            x:xs -> do
                put $ cur { testDCurData = xs }
                return $ Just x
            [] -> return Nothing

    rawCurClose = return ()
                        
mkTestDataStage :: [Row] -> Stage
mkTestDataStage rows = Stage
    { stgNewCursor = return $ toCursor $ TestDataCursor rows
    }
