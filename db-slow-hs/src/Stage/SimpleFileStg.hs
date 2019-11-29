module Stage.SimpleFileStg where

-- data SimpleFileCursor = SimpleFileCursor { fileIdx :: Int }

-- instance RawCursor SimpleFileCursor where
--     rawCurNext = do
--         -- update cursor
--         x <- get
--         put $ x { fileIdx = (fileIdx x) + 1 }
--         -- read data
--         let _pfix = fileIdx x 
--             fpath = "data/part-" ++ (show _pfix)
--             in lift $ do
--                 exists <- SDir.doesFileExist fpath
--                 if exists then
--                     fmap (Just . V.singleton . BS8.pack) $ readFile fpath
--                 else
--                     return Nothing

--     -- Nothing to close
--     rawCurClose = return ()

-- -- >>> printCursor $ toCursor SimpleFileCursor { fileIdx = 0 }
-- -- "xxxx"
-- -- "yyyyyy"
-- --