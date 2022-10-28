import Test.Tasty
import Test.Tasty.HUnit

import Lib2 (renderDocument, gameStart, hint)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  gameStartTests,
  hintTests])

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

listOfInts :: String
listOfInts = unlines [
      "---"
    , "- 5"
    , "- 6"
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" 
[   testCase "not dmap provided" $
      gameStart DList [DInteger 1] @?= "DMap expected at gameStart"
  , testCase "no \"number_of_hints\"" $
      gameStart DMap [("occupied_cols", DNull), ("occupied_rows", DNull)] @?= "Element with key \"number_of_hints\" not found in gameStart document"
  , testCase "no \"occupied_cols\"" $
      gameStart DMap [("number_of_hints", DInteger 10), ("occupied_rows", DNull)] @?= "Element with key \"occupied_cols\" not found in gameStart document"
  , testCase "no \"occupied_rows\"" $
      gameStart DMap [("number_of_hints", DInteger 10), ("occupied_cols", DNull)] @?= "Element with key \"occupied_rows\" not found in gameStart document"
  , testCase "no DInteger in number of hints" $
      gameStart DMap [("number_of_hints", String "test")] @?= "DInteger expected in \"number_of_hints\""
]

hintTests :: TestTree
hintTests = testGroup "Test hint document" []