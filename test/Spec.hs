import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encode )

import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..))
import Lib1 (State(..), Cell(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (cs (Y.encode doc)) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "null" $
        parseDocument "null" @?= Right DNull
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "string" $
        renderDocument (DString "5") @?= "5" 
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

initialState :: State
initialState = State {
  rowData = [],
  colData = [],
  document = DNull,
  board = take 100 (repeat Blank),
  hint_number = 3
}

finalGameStartState :: State
finalGameStartState = State {
  rowData = [2,0,2,2,2,0,6,0,3,3],
  colData = [1,1,2,3,1,4,2,4,2,0],
  document = DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DNull)])])])])])])])])])]),("occupied_rows",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 6),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 3),("tail",DNull)])])])])])])])])])]),("game_setup_id",DString "31f1c720-e0e7-47e7-be5c-a94d32e1088d")],
  board = take 100 (repeat Blank),
  hint_number = 10
}

finalHintState :: State
finalHintState = State {
  rowData = [],
  colData = [],
  document = DNull,
  board = [Ship] ++ take 99 (repeat Blank),
  hint_number = 3
}

getLeft :: Either String b -> String
getLeft (Left a) = a
getLeft (Right _) = "Expected Left"

getRight :: Either String b -> b
getRight (Right a) = a
getRight _ = error "No Right value provided"

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document"
  [   testCase "not dmap provided" $
        getLeft (gameStart initialState (DList [DInteger 1])) @?= "DMap expected at gameStart"
    , testCase "no \"number_of_hints\"" $
        getLeft (gameStart initialState (DMap [("occupied_cols", DNull), ("occupied_rows", DNull)])) @?= "Element with key \"number_of_hints\" not found in gameStart document"
    , testCase "no DInteger in number of hints" $
        getLeft (gameStart initialState (DMap [("number_of_hints", DString "test")])) @?= "DInteger expected in \"number_of_hints\""
    , testCase "no \"occupied_cols\"" $
        getLeft (gameStart initialState (DMap [("number_of_hints", DInteger 10), ("occupied_rows", DNull)])) @?= "Element with key \"occupied_cols\" not found in gameStart document"
    , testCase "no \"occupied_rows\"" $
        getLeft (gameStart initialState (DMap [("number_of_hints", DInteger 10), ("occupied_cols", DNull)])) @?= "Element with key \"occupied_rows\" not found in gameStart document"
    , testCase "no DMap for headTail" $
        getLeft (gameStart initialState (DMap [("number_of_hints",DInteger 10),("occupied_cols",DList [DInteger 1]), ("occupied_rows",DList [DInteger 1])])) @?= "row and col info in the gameStart document must be comprised of DMaps"
    , testCase "wrong \"head\" key" $
        getLeft (gameStart initialState (DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("not_head",DInteger 1),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DNull)])])]), ("occupied_rows",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])) @?= "wrong key"
    , testCase "\"head\" is not DInteger" $
        getLeft (gameStart initialState (DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("head",DString "test"),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DNull)])])]), ("occupied_rows",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])) @?= "\"head\" element must be an DInteger"
    , testCase "\"head\" integer is more than 10" $
        getLeft (gameStart initialState (DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("head",DInteger 11),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DNull)])])]), ("occupied_rows",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])) @?= "\"head\" element must be a DInteger with value between 0 and 10 (inclusive)"
    , testCase "wrong \"tail\" key" $
        getLeft (gameStart initialState (DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("head",DInteger 1),("not_tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DNull)])])]), ("occupied_rows",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])) @?= "wrong key"
    , testCase "\"tail\" is not DMap or DNull" $
        getLeft (gameStart initialState (DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 1),("tail", DString "test")])]), ("occupied_rows",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])) @?= "\"tail\" element must be an DMap or DNull"
    , testCase "too few col numbers" $
        getLeft (gameStart initialState (DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DNull)])])]), ("occupied_rows",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])) @?= "there must be 10 values in the occupied_cols element"
    , testCase "too few row numbers" $
        getLeft (gameStart initialState (DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DNull)])])])])])])])])])]), ("occupied_rows",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])) @?= "there must be 10 values in the occupied_rows element"
    , testCase "everything works in gameStart" $
        getRight (gameStart initialState (DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DNull)])])])])])])])])])]),("occupied_rows",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 6),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 3),("tail",DNull)])])])])])])])])])]),("game_setup_id",DString "31f1c720-e0e7-47e7-be5c-a94d32e1088d")])) @?= finalGameStartState
 
  ]

-- DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DNull)])])]), ("occupied_rows",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DMap)])])])]

hintTests :: TestTree
hintTests = testGroup "Test hint document" 
  [   
      testCase "key is not \"coords\"" $
        getLeft (hint initialState (DMap [("not_coords",DList [DMap [("col",DInteger 8),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 6)]])])) @?= "wrong key"
    , testCase "key is not \"col\"" $
        getLeft (hint initialState (DMap [("coords",DList [DMap [("not_col",DInteger 8),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 6)]])])) @?= "wrong key"
    , testCase "key is not \"row\"" $
        getLeft (hint initialState (DMap [("coords",DList [DMap [("col",DInteger 8),("not_row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 6)]])])) @?= "wrong key"
    , testCase "integer is out of bounds" $
        getLeft (hint initialState (DMap [("coords",DList [DMap [("col",DInteger 11),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 6)]])])) @?= "integer is out of bounds"
    , testCase "too many hints" $
        getLeft (hint initialState (DMap [("coords",DList [DMap [("col",DInteger 8),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 8),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 6)]])])) @?= "Wrong quantity of hints returned"
    , testCase "everything works in hint" $
        getRight (hint initialState (DMap [("coords",DList [DMap [("col",DInteger 0),("row",DInteger 0)]])])) @?= finalHintState
  ]

-- DMap [("coords",DList [DMap [("col",DInteger 8),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 6)]])]