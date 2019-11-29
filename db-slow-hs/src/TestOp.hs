module TestOp where

ops = []

parse :: [Op] -> Parser
parse ops =
    -- [(curr, prev)]
    processUnary xs =
        let xsWithPrevs = zip xs (Nothing:(fmap Just xs)) in
            foldr proc' (Right []) xsWithPrevs
        where
            proc' (curr, prev) (Right out) =
                if _isEmptyOrOp prev && _isUnaryOp x then
                    case out of
                        -- pop, apply unary and push back
                        i:out -> Right (mkNode x i):out
                        -- illegal
                        [] -> Left "unary op with no operand"
                else
                    Right x:out
            proc' _ ret@(Left _) = ret
            _isEmptyOrOp Nothing = True
            _isEmptyOrOp (Just op) | isOp op = True
            _isEmptyOrOp _ = False
            _isOp
        lefts