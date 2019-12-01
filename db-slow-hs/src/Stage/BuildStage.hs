module Stage.BuildStage where

import Control.Monad.Except (throwError)

import Planner (SqlPlan (..), SqlPlan' (..), TempSchema (..))
import Stage.FilterStage (mkFilterStage)
import Stage.ProjStage (mkProjStage)
import Expr.Def (SqlExpr)
import Def

buildStage :: SqlTableRepo -> SqlPlan -> ExceptS Stage
buildStage tblRepo plan =
    case plan' of
        SPProj exprs subplan -> do
            subStage <- buildStage tblRepo subplan
            return $ mkProjStage (getLookUpCol subplan) exprs subStage
        SPFilter expr subplan -> do
            subStage <- buildStage tblRepo subplan
            return $ mkFilterStage (getLookUpCol subplan) expr subStage
        SPTable tbl ->
            case lookUpTable tblRepo tbl of
                Just tblDef -> do
                    return $ tblStage tblDef
                Nothing -> throwError $ "Table not found: " ++ tbl
    where
        getLookUpCol = lookUpCol . sPlanSchema
        plan' = sPlanP plan

-- mkProjStage :: FnLookUpCol -> [SqlExpr] -> Stage -> ExceptS Stage
-- mkProjStage _ _ _ = throwError "ProjStage not implemented"

-- mkFilterStage :: FnLookUpCol -> SqlExpr -> Stage -> ExceptS Stage
-- mkFilterStage _ _ _ = throwError "FilterStage not implemented"
