module Feature.RollbackSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

-- two helpers functions to make sure that each test can setup and cleanup properly

-- creates Item to work with for PATCH and DELETE
postItem =
  request methodPost "/items"
      [("Prefer", "tx=commit"), ("Prefer", "resolution=ignore-duplicates")]
      [json|{"id":0}|]
    `shouldRespondWith`
      ""
      { matchStatus  = 201
      , matchHeaders = [matchHeaderAbsent hContentType]  }

-- removes Items left over from POST, PUT, and PATCH
deleteItems =
  request methodDelete "/items?id=lte.0"
      [("Prefer", "tx=commit")]
      ""
    `shouldRespondWith`
      ""
      { matchStatus  = 204
      , matchHeaders = [matchHeaderAbsent hContentType] }

preferDefault  = [("Prefer", "return=representation")]
preferCommit   = [("Prefer", "return=representation"), ("Prefer", "tx=commit")]
preferRollback = [("Prefer", "return=representation"), ("Prefer", "tx=rollback")]

withoutPreferenceApplied      = []
withPreferenceCommitApplied   = [ matchHeaderValuePresent "Preference-Applied" "tx=commit" ]
withPreferenceRollbackApplied = [ matchHeaderValuePresent "Preference-Applied" "tx=rollback" ]

shouldRespondToReads reqHeaders respHeaders = do
  it "responds to GET" $ do
    request methodGet "/items?id=eq.1"
        reqHeaders
        ""
      `shouldRespondWith`
        [json|[{"id":1}]|]
        { matchHeaders = respHeaders }

  it "responds to HEAD" $ do
    request methodHead "/items?id=eq.1"
        reqHeaders
        ""
      `shouldRespondWith`
        ""
        { matchHeaders = matchContentTypeJson : respHeaders }

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
    preferDefault `shouldRespondToReads` withoutPreferenceApplied
    preferDefault `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=commit" $ do
    preferCommit `shouldRespondToReads` withPreferenceCommitApplied
    -- Exceptions are always without preference applied,
    -- because they return before the end of the transaction.
    preferCommit `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=rollback" $ do
    preferRollback `shouldRespondToReads` withPreferenceRollbackApplied
    -- Exceptions are always without preference applied,
    -- because they return before the end of the transaction.
    preferRollback `shouldRaiseExceptions` withoutPreferenceApplied

disallowed :: SpecWith ((), Application)
disallowed = describe "tx-rollback-all = false, tx-allow-override = false" $ do
  describe "without Prefer tx" $ do
    preferDefault `shouldRespondToReads` withoutPreferenceApplied
    preferDefault `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=commit" $ do
    preferCommit `shouldRespondToReads` withoutPreferenceApplied
    preferCommit `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=rollback" $ do
    preferRollback `shouldRespondToReads` withoutPreferenceApplied
    preferRollback `shouldRaiseExceptions` withoutPreferenceApplied


forced :: SpecWith ((), Application)
forced = describe "tx-rollback-all = true, tx-allow-override = false" $ do
  describe "without Prefer tx" $ do
    preferDefault `shouldRespondToReads` withoutPreferenceApplied
    preferDefault `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=commit" $ do
    preferCommit `shouldRespondToReads` withoutPreferenceApplied
    preferCommit `shouldRaiseExceptions` withoutPreferenceApplied

  describe "Prefer tx=rollback" $ do
    preferRollback `shouldRespondToReads` withoutPreferenceApplied
    preferRollback `shouldRaiseExceptions` withoutPreferenceApplied

