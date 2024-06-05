module Feature.Query.AggregateFunctionsSpec where

import Network.Wai (Application)

import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

allowed :: SpecWith ((), Application)
allowed =
  describe "aggregate functions" $ do
    context "performing an aggregation on one or more fields" $ do
      it "supports use of aggregates on RPC functions that return table values" $
        get "/rpc/getallprojects?select=id.max()" `shouldRespondWith`
          [json|[{"max": 5}]|] { matchHeaders = [matchContentTypeJson] }
