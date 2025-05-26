module Feature.RollbackSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

preferDefault  = [("Prefer", "return=representation")]
preferCommit   = [("Prefer", "return=representation"), ("Prefer", "tx=commit")]
preferRollback = [("Prefer", "return=representation"), ("Prefer", "tx=rollback")]

withoutPreferenceApplied      = []
withPreferenceCommitApplied   = [ matchHeaderValuePresent "Preference-Applied" "tx=commit" ]
withPreferenceRollbackApplied = [ matchHeaderValuePresent "Preference-Applied" "tx=rollback" ]

shouldRespondToRPCReads reqHeaders respHeaders = do
  it "responds to GET on RPC" $ do
    request methodGet "/rpc/search?id=1"
        reqHeaders
        ""
      `shouldRespondWith`
        [json|[{"id":1}]|]
        { matchHeaders = respHeaders }

  it "responds to POST on RPC" $ do
    request methodPost "/rpc/search"
        reqHeaders
        [json|{"id":1}|]
      `shouldRespondWith`
        [json|[{"id":1}]|]
        { matchHeaders = respHeaders }

shouldRaiseExceptions reqHeaders respHeaders = do
  it "raises immediate constraints" $ do
    request methodPost "/rpc/raise_constraint"
        reqHeaders
        ""
      `shouldRespondWith`
        [json|{
          "hint":null,
          "details":"Key (col)=(1) already exists.",
          "code":"23505",
          "message":"duplicate key value violates unique constraint \"deferrable_unique_constraint_col_key\""
        }|]
        { matchStatus = 409
        , matchHeaders = respHeaders }

  it "raises deferred constraints" $ do
    request methodPost "/rpc/raise_constraint"
        reqHeaders
        [json|{"deferred": true}|]
      `shouldRespondWith`
        [json|{
          "hint":null,
          "details":"Key (col)=(1) already exists.",
          "code":"23505",
          "message":"duplicate key value violates unique constraint \"deferrable_unique_constraint_col_key\""
        }|]
        { matchStatus = 409
        , matchHeaders = respHeaders }

allowed :: SpecWith ((), Application)
allowed = describe "tx-allow-override = true" $ do
  describe "without Prefer tx" $ do
    preferDefault `shouldRespondToRPCReads` withoutPreferenceApplied
    preferDefault `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=commit" $ do
    preferCommit `shouldRespondToRPCReads` withPreferenceCommitApplied
    -- Exceptions are always without preference applied,
    -- because they return before the end of the transaction.
    preferCommit `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=rollback" $ do
    preferRollback `shouldRespondToRPCReads` withPreferenceRollbackApplied
    -- Exceptions are always without preference applied,
    -- because they return before the end of the transaction.
    preferRollback `shouldRaiseExceptions` withoutPreferenceApplied

disallowed :: SpecWith ((), Application)
disallowed = describe "tx-rollback-all = false, tx-allow-override = false" $ do
  describe "without Prefer tx" $ do
    preferDefault `shouldRespondToRPCReads` withoutPreferenceApplied
    preferDefault `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=commit" $ do
    preferCommit `shouldRespondToRPCReads` withoutPreferenceApplied
    preferCommit `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=rollback" $ do
    preferRollback `shouldRespondToRPCReads` withoutPreferenceApplied
    preferRollback `shouldRaiseExceptions` withoutPreferenceApplied


forced :: SpecWith ((), Application)
forced = describe "tx-rollback-all = true, tx-allow-override = false" $ do
  describe "without Prefer tx" $ do
    preferDefault `shouldRespondToRPCReads` withoutPreferenceApplied
    preferDefault `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=commit" $ do
    preferCommit `shouldRespondToRPCReads` withoutPreferenceApplied
    preferCommit `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=rollback" $ do
    preferRollback `shouldRespondToRPCReads` withoutPreferenceApplied
    preferRollback `shouldRaiseExceptions` withoutPreferenceApplied