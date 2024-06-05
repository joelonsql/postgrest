module Feature.ExtraSearchPathSpec where

import Network.HTTP.Types
import Network.Wai         (Application)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "extra search path" $ do

  it "finds the isn is_valid function on the extensions schema" $
    request methodGet "/rpc/is_valid_isbn?input=978-0-393-04002-9" [] ""
      `shouldRespondWith` [json|true|]
      { matchHeaders = [matchContentTypeJson] }

  it "finds a function on a schema with uppercase and special characters in its name" $
    request methodGet "/rpc/special_extended_schema?val=value" [] ""
      `shouldRespondWith` [json|"value"|]
      { matchHeaders = [matchContentTypeJson] }
