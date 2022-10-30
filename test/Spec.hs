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
    , testCase "string" $
        renderDocument (DString "5") @?= string 
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    , testCase "list of miscellaneous primitives" $
        renderDocument (DList [DInteger 5, DNull]) @?= "---\n- 5\n- null\n"
    , testCase "list in a list" $
        renderDocument (DList [DList [DInteger 1 , DInteger 2], DList [DInteger 3, DInteger 4]]) @?= "---\n- \n  - 1\n  - 2\n- \n  - 3\n  - 4\n"
    , testCase "empty list" $
        renderDocument (DList []) @?= "---\n"
    , testCase "DMap in a DList." $
        renderDocument (DList [DMap[("key", DInteger 5), ("key2", DInteger 4)]]) @?= "---\n- \n  key: 5\n  key2: 4\n"
    , testCase "Few DMaps in a DList. DList [DMap (\"key\", value)]" $
        renderDocument (DList [DMap[("key", DInteger 5)], DMap[("key2", DInteger 5)]]) @?= "---\n- \n  key: 5\n- \n  key2: 5\n"
    , testCase "Empty Dmap" $
        renderDocument (DList []) @?= "---\n"
    , testCase "String in DMap" $
        renderDocument (DMap [("key", DString "5")]) @?= "---\nkey: 5\n"
    , testCase "Integer in DMap" $
        renderDocument (DMap [("key", DInteger 5)]) @?= "---\nkey: 5\n"
    , testCase "null in DMap" $
        renderDocument (DMap [("key", DNull)]) @?= "---\nkey: null\n"
    , testCase "DList in DMap" $
        renderDocument (DMap [("key", DList[DInteger 5])]) @?= "---\nkey: \n  - 5\n"
    , testCase "DMap in DMap" $
        renderDocument (DMap [("key1", DMap[("key2", DList[DInteger 5])])]) @?= "---\nkey1: \n  key2: \n    - 5\n"
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

string :: String
string = unlines [
      "5"
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" []

hintTests :: TestTree
hintTests = testGroup "Test hint document" []