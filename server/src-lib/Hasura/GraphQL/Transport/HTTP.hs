module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import qualified Control.Concurrent.Async.Lifted        as A
import           Control.Monad.Trans.Control
import qualified Data.Sequence                          as Seq
import qualified Data.Text                              as T
import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (RequestId)

import qualified Hasura.GraphQL.Execute                 as E

runGQ
  :: ( MonadIO m
     , MonadBaseControl IO m
     -- , A.Forall (A.Pure m)  -- TODO I think we should be able to use Async.Lifted.Safe
     -- ^ ceremony for lifted-async
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [N.Header]
  -> GQLReqUnparsed
  -> m (HttpResponse EncJSON)
runGQ reqId userInfo reqHdrs req = do
  E.ExecutionCtx _ sqlGenCtx pgExecCtx planCache sc scVer _ enableAL <- ask
  fieldPlans <- E.getResolvedExecPlan pgExecCtx planCache
              userInfo sqlGenCtx enableAL sc scVer req
  -- do all top-level data fetching concurrently:
  -- TODO consider:
  --  - haxl for batching + concurrency
  --  - or at least manually limiting concurrncy here (in case user has dozens of fields)
  -- FIXME we need to run mutations serially; to do that cleanly (and since we should probably do this anyway) we want to move the query/mutation/subscription tag out of each individual field into a single parent data type (i.e. it's unclear at this call site that all fields are going to be the same op type)
  fieldResps <- A.forConcurrently fieldPlans $ \case
    E.GQFieldResolvedHasura resolvedOp ->
      flip HttpResponse Nothing <$> runHasuraGQ reqId req userInfo resolvedOp
    E.GQFieldResolvedRemote rsi opType field ->
      E.execRemoteGQ reqId userInfo reqHdrs rsi opType (Seq.singleton field)

  let mergedResp = mergeResponses (fmap _hrBody fieldResps)
  case mergedResp of
    Left e ->
      throw400
        UnexpectedPayload
        ("could not merge data from results: " <> T.pack e)
    Right mergedGQResp ->
      pure (HttpResponse mergedGQResp (foldMap _hrHeaders fieldResps))

runHasuraGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> E.ExecOp
  -> m EncJSON
runHasuraGQ reqId query userInfo resolvedOp = do
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _ <- ask
  respE <- liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx genSql  -> do
      -- log the generated SQL and the graphql query
      liftIO $ logGraphqlQuery logger $ QueryLog query genSql reqId
      runLazyTx' pgExecCtx tx
    E.ExOpMutation tx -> do
      -- log the graphql query
      liftIO $ logGraphqlQuery logger $ QueryLog query Nothing reqId
      runLazyTx pgExecCtx $ withUserInfo userInfo tx
    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftEither respE
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp
