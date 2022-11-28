import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)

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

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)


golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
        \doc -> parseDocument (friendlyEncode doc) == Right doc
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
    , testCase "int" $
        parseDocument "5" @?= Right (DInteger 5)
    , testCase "string" $
        parseDocument "\"5\"" @?= Right (DString "5") 
    , testCase "list of ints" $              
        parseDocument listOfInts @?= Right (DList [DInteger 5, DInteger 6])
    , testCase "list of miscellaneous primitives" $
        parseDocument listOfRandom @?= Right (DList [DInteger 5, DString "5", DNull])
    , testCase "list in a list" $
        parseDocument listInList @?= Right (DList [DList [DInteger 1 , DInteger 2], DList [DInteger 3, DInteger 4, DList [DInteger 5, DInteger 5]]])
    , testCase "DMap in a DList." $
        parseDocument oneDMapinDList' @?= Right (DList [DMap[("key", DInteger 5), ("key", DString "4")]])
    , testCase "Few DMaps in a DList" $
        parseDocument fewDMapsinDList' @?= Right (DList [DMap[("key", DInteger 5)], DMap[("key", DString "4")]])
    , testCase "DList -> DMap -> DList" $
        parseDocument listmaplist @?= Right (DList [DMap[("key",DList[DInteger 5])]])
    , testCase "primitives/more than one mapping in DMap" $
        parseDocument aFewMappings' @?= Right (DMap [("key", DInteger 5), ("key", DString "5"), ("key", DNull)])
    , testCase "DMap -> DList" $           
        parseDocument "---\nkey: \n  - 5\n" @?= Right (DMap [("key", DList[DInteger 5])])
    , testCase "DMap -> DList -> DMap" $           
        parseDocument maplistmap' @?= Right (DMap [("KEY", DList[DMap[("key", DNull), ("key", DNull), ("key", DList[DInteger 1, DInteger 2])]])])
    , testCase "Simple DMap -> DMap" $            
        parseDocument "---\nkey: \n  key: null\n" @?= Right (DMap [("key", DMap[("key", DNull)])])
    , testCase "Complicated DMap -> DMap" $   
         parseDocument cmp' @?= Right (DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DNull)])])])])])])])])])]),("occupied_rows",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 6),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 3),("tail",DNull)])])])])])])])])])]),("game_setup_id",DString "31f1c720_e0e7_47e7_be5c_a94d32e1088d")])
    , testCase "Coords Dmap" $            
        parseDocument cmp2 @?= Right (DMap [("coords",DList [DMap [("col",DInteger 8),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 6),("row",DInteger 6)],DMap [("col",DInteger 5),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 4)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 2)],DMap [("col",DInteger 4),("row",DInteger 9)],DMap [("col",DInteger 5),("row",DInteger 9)],DMap [("col",DInteger 6),("row",DInteger 9)]])])
    , testCase "Coords Dmap" $            
        parseDocument cmp22 @?= Right (DMap [("coords",DList [DMap [("col",DInteger 8),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 6),("row",DInteger 6)],DMap [("col",DInteger 5),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 4)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 2)],DMap [("col",DInteger 4),("row",DInteger 9)],DMap [("col",DInteger 5),("row",DInteger 9)],DMap [("col",DInteger 6),("row",DInteger 9)]])])
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

oneDMapinDList' :: String 
oneDMapinDList' = unlines [
    "---" 
   ,"- " 
   ," key: 5"
   ," key: \"4\""
 ]

fewDMapsinDList' :: String 
fewDMapsinDList' = unlines [
    "---"
   ,"- "
   ," key: 5"
   ,"- "
   ," key: \"4\""
 ]

aFewMappings' :: String 
aFewMappings'  = unlines [
   "---"
  ,"key: 5"
  ,"key: \"5\""
  ,"key: null"
 ]

maplistmap' :: String 
maplistmap' = unlines [
    "---"
   ,"KEY: "
   ," - "
   ,"  key: null"
   ,"  key: null"
   ,"  key: "
   ,"   - 1"
   ,"   - 2"
 ]

cmp' :: String 
cmp' = unlines [
   "---"
   ,"number_of_hints: 10"
   ,"occupied_cols: "
   ," head: 1"
   ," tail: "
   ,"  head: 1"
   ,"  tail: "
   ,"   head: 2"
   ,"   tail: "
   ,"    head: 3"
   ,"    tail: "
   ,"     head: 1"
   ,"     tail: "
   ,"      head: 4"
   ,"      tail: "
   ,"       head: 2"
   ,"       tail: "
   ,"        head: 4"
   ,"        tail: "
   ,"         head: 2"
   ,"         tail: "
   ,"          head: 0"
   ,"          tail: null"
   ,"occupied_rows: "
   ," head: 2"
   ," tail: "
   ,"  head: 0"
   ,"  tail: "
   ,"   head: 2"
   ,"   tail: "
   ,"    head: 2"
   ,"    tail: "
   ,"     head: 2"
   ,"     tail: "
   ,"      head: 0"
   ,"      tail: "
   ,"       head: 6"
   ,"       tail: "
   ,"        head: 0"
   ,"        tail: "
   ,"         head: 3"
   ,"         tail: "
   ,"          head: 3"
   ,"          tail: null"
   ,"game_setup_id: \"31f1c720_e0e7_47e7_be5c_a94d32e1088d\""
  ]

cmp22 :: String
cmp22 = unlines [
   "---"
   ,"coords: "
   ,"- col: 8"
   ,"  row: 6"
   ,"- col: 7"
   ,"  row: 6"
   ,"- col: 6"
   ,"  row: 6"
   ,"- col: 5"
   ,"  row: 6"
   ,"- col: 7"
   ,"  row: 4"
   ,"- col: 7"
   ,"  row: 3"
   ,"- col: 7"
   ,"  row: 2"
   ,"- col: 4"
   ,"  row: 9"
   ,"- col: 5"
   ,"  row: 9"
   ,"- col: 6"
   ,"  row: 9"
  ]

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $                      -- primitives
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "string" $
        renderDocument (DString "5") @?= "\"5\"" 
    , testCase "list of ints" $              -- list and nested lists
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    , testCase "list of miscellaneous primitives" $
        renderDocument (DList [DInteger 5, DString "5", DNull]) @?= listOfRandom
    , testCase "list in a list" $
        renderDocument (DList [DList [DInteger 1 , DInteger 2], DList [DInteger 3, DInteger 4, DList [DInteger 5, DInteger 5]]]) @?= listInList
    , testCase "list in a list + primitive" $
        renderDocument (DList [DList [DInteger 1 , DInteger 2], DNull]) @?= nestedListandPrim
    , testCase "empty list" $
        renderDocument (DList []) @?= "[]\n"
    , testCase "DMap in a DList." $
        renderDocument (DList [DMap[("key", DInteger 5), ("key2", DString "4")]]) @?= oneDMapinDList
    , testCase "Few DMaps in a DList" $
        renderDocument (DList [DMap[("key", DInteger 5)], DMap[("key2", DString "4")]]) @?= fewDMapsinDList
    , testCase "DList -> DMap -> DList" $
         renderDocument (DList [DMap[("key",DList[DInteger 5])]])  @?= listmaplist
    , testCase "Empty Dmap" $               -- Dmaps with primitives
        renderDocument (DMap []) @?= "{}\n"
    , testCase "String in DMap" $
        renderDocument (DMap [("key", DString "5")]) @?= "---\nkey: \"5\"\n"
    , testCase "Integer in DMap" $
        renderDocument (DMap [("key", DInteger 5)]) @?= "---\nkey: 5\n"
    , testCase "null in DMap" $
        renderDocument (DMap [("key", DNull)]) @?= "---\nkey: null\n"
    , testCase "primitives/more than one mapping in DMap" $
        renderDocument (DMap [("key1", DInteger 5), ("key2", DString "5"), ("key3", DNull)]) @?= aFewMappings
    , testCase "DMap -> DList" $           -- DMaps with DLists 
        renderDocument (DMap [("key", DList[DInteger 5])]) @?= "---\nkey: \n - 5\n"
    , testCase "DMap -> DList -> DMap" $           
        renderDocument (DMap [("KEY", DList[DMap[("key1", DNull), ("key2", DNull), ("key3", DList[DInteger 1, DInteger 2])]])]) @?= maplistmap
    , testCase "Simple DMap -> DMap" $            -- DMaps with DMaps
        renderDocument (DMap [("key1", DMap[("key2", DNull)])]) @?= "---\nkey1: \n key2: null\n"
    , testCase "Coords Dmap" $            
        renderDocument (DMap [("coords",DList [DMap [("col",DInteger 8),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 6),("row",DInteger 6)],DMap [("col",DInteger 5),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 4)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 2)],DMap [("col",DInteger 4),("row",DInteger 9)],DMap [("col",DInteger 5),("row",DInteger 9)],DMap [("col",DInteger 6),("row",DInteger 9)]])]) @?= cmp2
    , testCase "Tricky case" $          
        renderDocument trickyCaseDocument @?= trickyCaseString
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

trickyCaseDocument :: Document
trickyCaseDocument =
 DMap [
    ("key1", DMap [
        ("key2", DList [
            DInteger 1,
            DMap [
                ("key3", DList [
                    DInteger 1,
                    DInteger 3,
                    DNull,
                    DMap [("", DNull)],
                    DMap []
                ]),
                ("key4", DString "")],
            DNull
        ])
    ]),
    ("key5", DList [])
 ]

trickyCaseString :: String
trickyCaseString = unlines [
   "---",
   "key1: ",
   " key2: ",
   "  - 1",
   "  - ",
   "  key3: ",
   "   - 1",
   "   - 3",
   "   - null",
   "   - ",
   "    '': null",
   "   - {}",
   "   key4: ''",  
   "  - null",
   "key5: []"
 ]

listOfInts :: String
listOfInts = unlines [
      "---"
    , "- 5"
    , "- 6"
  ]

listOfRandom :: String
listOfRandom = unlines [
      "---"
    , "- 5"
    , "- \"5\""
    , "- null"
  ]
  
listInList :: String 
listInList = unlines [
     "---"
   , "- "
   , " - 1"
   , " - 2"
   , "- "
   , " - 3"
   , " - 4"
   , " - "
   , "  - 5"
   , "  - 5"
  ]

nestedListandPrim :: String 
nestedListandPrim = unlines [
    "---"
    ,"- "
    ," - 1"
    ," - 2"
    ,"- null"
  ]

oneDMapinDList :: String 
oneDMapinDList = unlines [
    "---" 
   ,"- " 
   ," key: 5"
   ," key2: \"4\""
 ]

fewDMapsinDList :: String 
fewDMapsinDList = unlines [
    "---"
   ,"- "
   ," key: 5"
   ,"- "
   ," key2: \"4\""
 ]

aFewMappings :: String 
aFewMappings  = unlines [
   "---"
  ,"key1: 5"
  ,"key2: \"5\""
  ,"key3: null"
 ]

listmaplist :: String 
listmaplist = unlines [
    "---"
    ,"- "
    ," key: "
    ,"  - 5"
 ] 

maplistmap :: String 
maplistmap = unlines [
    "---"
   ,"KEY: "
   ," - "
   ,"  key1: null"
   ,"  key2: null"
   ,"  key3: "
   ,"   - 1"
   ,"   - 2"
 ]

cmp2 :: String
cmp2 = unlines [
   "---"
   ,"coords: "
   ," - "
   ,"  col: 8"
   ,"  row: 6"
   ," - "
   ,"  col: 7"
   ,"  row: 6"
   ," - "
   ,"  col: 6"
   ,"  row: 6"
   ," - "
   ,"  col: 5"
   ,"  row: 6"
   ," - "
   ,"  col: 7"
   ,"  row: 4"
   ," - "
   ,"  col: 7"
   ,"  row: 3"
   ," - "
   ,"  col: 7"
   ,"  row: 2"
   ," - "
   ,"  col: 4"
   ,"  row: 9"
   ," - "
   ,"  col: 5"
   ,"  row: 9"
   ," - "
   ,"  col: 6"
   ,"  row: 9"
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