{-# LANGUAGE StandaloneDeriving #-}
import Test.HUnit
import System.IO
import Control.Exception (evaluate)
import qualified Data.Set as Set
import Data.Char ( toUpper, toLower )
import Data.List (sort)
import qualified HW2
import Test.HUnit (assertEqual)
import System.Timeout


testWithTimeout :: (Eq a, Show a) => Int -> a -> a -> Assertion
testWithTimeout time expected action = do
  result <- timeout time (evaluate $ action)
  case result of
    Just value -> assertEqual "" expected value
    Nothing -> assertEqual "Timeout expired" False True

smallSample = take 5 . HW2.itoList
sample = take 10 . HW2.itoList



-- Test cases
testIsPrefixOf :: Test
testIsPrefixOf =
  test
    [ "Test 1: HW2.isPrefixOf \"abc\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) True (HW2.isPrefixOf "abc" "abcdef")
    , "Test 2: HW2.isPrefixOf \"xyz\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) False (HW2.isPrefixOf "xyz" "abcdef")
    , "Test 3: HW2.isPrefixOf \"\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) True (HW2.isPrefixOf "" "abcdef") 
    , "Test 4: HW2.isPrefixOf \"abc\" \"\"" ~: testWithTimeout (1 * 1000000) False (HW2.isPrefixOf "abc" "")
    , "Test 5: HW2.isPrefixOf \"abcdefg\" \"abc\"" ~: testWithTimeout (1 * 1000000) False (HW2.isPrefixOf "abcdefg" "abc") 
    , "Test 6: HW2.isPrefixOf \"\" \"\"" ~: testWithTimeout (1 * 1000000) True (HW2.isPrefixOf "" "")
    , "Test 7: HW2.isPrefixOf \"abcdef\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) True (HW2.isPrefixOf "abcdef" "abcdef") 
    , "Test 8: HW2.isPrefixOf \"abc\" \"abcabcabc\"" ~: testWithTimeout (1 * 1000000) True (HW2.isPrefixOf "abc" "abcabcabc")
    , "Test 9: HW2.isPrefixOf \"  Hello\" \"  Hello, World!\"" ~: testWithTimeout (1 * 1000000) True (HW2.isPrefixOf "  Hello" "  Hello, World!")
    ]


testIsSuffixOf :: Test
testIsSuffixOf =
  "Test isSuffixOf" ~:
    TestList
      [ "Test 1: HW2.isSuffixOf \"def\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) True (HW2.isSuffixOf "def" "abcdef")
      , "Test 2: HW2.isSuffixOf \"xyz\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) False (HW2.isSuffixOf "xyz" "abcdef")
      , "Test 3: HW2.isSuffixOf \"\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) True (HW2.isSuffixOf "" "abcdef")
      , "Test 4: HW2.isSuffixOf \"abc\" \"\"" ~: testWithTimeout (1 * 1000000) False (HW2.isSuffixOf "abc" "")
      , "Test 5: HW2.isSuffixOf \"abcdefg\" \"abc\"" ~: testWithTimeout (1 * 1000000) False (HW2.isSuffixOf "abcdefg" "abc")
      , "Test 6: HW2.isSuffixOf \"\" \"\"" ~: testWithTimeout (1 * 1000000) True (HW2.isSuffixOf "" "")
      , "Test 7: HW2.isSuffixOf \"abcdef\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) True (HW2.isSuffixOf "abcdef" "abcdef")
      , "Test 8: HW2.isSuffixOf \"abc\" \"abcabcabc\"" ~: testWithTimeout (1 * 1000000) True (HW2.isSuffixOf "abc" "abcabcabc")
      , "Test 9: HW2.isSuffixOf \"World!  \" \"  Hello, World!  \"" ~: testWithTimeout (1 * 1000000) True (HW2.isSuffixOf "World!  " "  Hello, World!  ")
      ]


testIsInfixOf :: Test
testIsInfixOf =
  test
      [ "Test 1: HW2.isInfixOf \"abc\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) True (HW2.isInfixOf "abc" "abcdef")
      , "Test 2: HW2.isInfixOf \"xyz\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) False (HW2.isInfixOf "xyz" "abcdef")
      , "Test 3: HW2.isInfixOf \"\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) True (HW2.isInfixOf "" "abcdef")
      , "Test 4: HW2.isInfixOf \"abc\" \"\"" ~: testWithTimeout (1 * 1000000) False (HW2.isInfixOf "abc" "")
      , "Test 5: HW2.isInfixOf \"abcdefg\" \"abc\"" ~: testWithTimeout (1 * 1000000) False (HW2.isInfixOf "abcdefg" "abc")
      , "Test 6: HW2.isInfixOf \"\" \"\"" ~: testWithTimeout (1 * 1000000) True (HW2.isInfixOf "" "")
      , "Test 7: HW2.isInfixOf \"abcdef\" \"abcdef\"" ~: testWithTimeout (1 * 1000000) True (HW2.isInfixOf "abcdef" "abcdef")
      , "Test 8: HW2.isInfixOf \"abc\" \"abcabcabc\"" ~: testWithTimeout (1 * 1000000) True (HW2.isInfixOf "abc" "abcabcabc")
      , "Test 9: HW2.isInfixOf \"abc\" \"kjhabcsdf\"" ~: testWithTimeout (1 * 1000000) True (HW2.isInfixOf "abc" "kjhabcsdf")
      , "Test 10: HW2.isInfixOf \"abc\" \"kjhaxbxcsdf\"" ~: testWithTimeout (1 * 1000000) False (HW2.isInfixOf "abc" "kjhaxbxcsdf")
      ]
 
testIsSubseuenceOf :: Test
testIsSubseuenceOf =
    TestList
      [ "Test 1: HW2.isSubseuenceOf \"abc\" \"abcdef\"" ~: testWithTimeout (1000000) True (HW2.isSubseuenceOf "abc" "abcdef")
      , "Test 2: HW2.isSubseuenceOf \"acb\" \"abcdef\"" ~: testWithTimeout (1000000) False (HW2.isSubseuenceOf "acb" "abcdef")
      , "Test 3: HW2.isSubseuenceOf \"\" \"abcdef\"" ~: testWithTimeout (1000000) True (HW2.isSubseuenceOf "" "abcdef")
      , "Test 4: HW2.isSubseuenceOf \"abc\" \"\"" ~: testWithTimeout (1000000) False (HW2.isSubseuenceOf "abc" "")
      , "Test 5: HW2.isSubseuenceOf \"abcdefg\" \"abc\"" ~: testWithTimeout (1000000) False (HW2.isSubseuenceOf "abcdefg" "abc")
      , "Test 6: HW2.isSubseuenceOf \"\" \"\"" ~: testWithTimeout (1000000) True (HW2.isSubseuenceOf "" "")
      , "Test 7: HW2.isSubseuenceOf \"abcdef\" \"abcdef\"" ~: testWithTimeout (1000000) True (HW2.isSubseuenceOf "abcdef" "abcdef")
      , "Test 8: HW2.isSubseuenceOf \"abc\" \"abcabcabc\"" ~: testWithTimeout (1000000) True (HW2.isSubseuenceOf "abc" "abcabcabc")
      , "Test 10: HW2.isSubseuenceOf \"Hello\" \"  Hello, World!\"" ~: testWithTimeout (1000000) True (HW2.isSubseuenceOf "Hello" "  Hello, World!")
      , "Test 11: HW2.isSubseuenceOf \"148\" \"12345678\"" ~: testWithTimeout (1000000) True (HW2.isSubseuenceOf "148" "12345678")
      , "Test 12: HW2.isSubseuenceOf \"238\" \"12345678\"" ~: testWithTimeout (1000000) True (HW2.isSubseuenceOf "238" "12345678")
      ]


testFindDocuments :: Test
testFindDocuments =
  test
      [ "Test 1: HW2.findDocuments (HW2.Literal \"a\") [\"abcd\", \"abef\", \"xyz\", \"abcdefg\"]" ~: testWithTimeout (1000000) (["abcd", "abef", "abcdefg"], ["xyz"]) (HW2.findDocuments (HW2.Literal "a") ["abcd", "abef", "xyz", "abcdefg"])
      , "Test 2: HW2.findDocuments (HW2.All [HW2.Literal \"a\", HW2.Literal \"b\", HW2.Literal \"c\"]) [\"abcd\", \"abef\", \"xyz\", \"abcdefg\"]" ~: testWithTimeout (1000000) (["abcd", "abcdefg"], ["abef", "xyz"]) (HW2.findDocuments (HW2.All [HW2.Literal "a", HW2.Literal "b", HW2.Literal "c"]) ["abcd", "abef", "xyz", "abcdefg"])
      , "Test 3: HW2.findDocuments (HW2.All []) [\"abcd\", \"abef\", \"xyz\", \"abcdefg\"]" ~: testWithTimeout (1000000) (["abcd", "abef", "xyz", "abcdefg"], []) (HW2.findDocuments (HW2.All []) ["abcd", "abef", "xyz", "abcdefg"])
      , "Test 4: HW2.findDocuments (HW2.All [HW2.Literal \"m\"]) [\"abcd\", \"abef\", \"xyz\", \"abcdefg\"]" ~: testWithTimeout (1000000) ([], ["abcd", "abef", "xyz", "abcdefg"]) (HW2.findDocuments (HW2.All [HW2.Literal "m"]) ["abcd", "abef", "xyz", "abcdefg"])
      , "Test 5: HW2.findDocuments (HW2.Any [HW2.Literal \"m\", HW2.Literal \"n\"]) [\"abcd\", \"abef\", \"xyz\", \"abcdefg\"]" ~: testWithTimeout (1000000) ([], ["abcd", "abef", "xyz", "abcdefg"]) (HW2.findDocuments (HW2.Any [HW2.Literal "m", HW2.Literal "n"]) ["abcd", "abef", "xyz", "abcdefg"])
      , "Test 6: HW2.findDocuments (HW2.Any []) [\"abcd\", \"abef\", \"xyz\", \"abcdefg\"]" ~: testWithTimeout (1000000) ([], ["abcd", "abef", "xyz", "abcdefg"]) (HW2.findDocuments (HW2.Any []) ["abcd", "abef", "xyz", "abcdefg"])
      , "Test 7: HW2.findDocuments (HW2.Any [HW2.Literal \"a\"]) [\"abcd\", \"abef\", \"axyz\", \"abcdefg\"]" ~: testWithTimeout (1000000) (["abcd", "abef", "axyz", "abcdefg"], []) (HW2.findDocuments (HW2.Any [HW2.Literal "a"]) ["abcd", "abef", "axyz", "abcdefg"])
      , "Test 8: HW2.findDocuments (HW2.Any [HW2.Literal \"a\", HW2.Literal \"g\"]) [\"abcd\", \"befg\", \"xyz\", \"abcdefg\"]" ~: testWithTimeout (1000000) (["abcd", "befg", "abcdefg"], ["xyz"]) (HW2.findDocuments (HW2.Any [HW2.Literal "a", HW2.Literal "g"]) ["abcd", "befg", "xyz", "abcdefg"])
      , "Test 9: HW2.findDocuments (HW2.Any [HW2.Literal \"xyz\", HW2.Literal \"def\"]) [\"abcd\", \"abef\", \"xyz\", \"abcdefg\"]" ~: testWithTimeout (1000000) (["xyz", "abcdefg"], ["abcd", "abef"]) (HW2.findDocuments (HW2.Any [HW2.Literal "xyz", HW2.Literal "def"]) ["abcd", "abef", "xyz", "abcdefg"])
      , "Test 10: HW2.findDocuments (HW2.None [HW2.None [HW2.Literal \"abc\"]]) [\"abcd\", \"abef\", \"xyz\", \"abcdefg\"]" ~: testWithTimeout (1000000) (["abcd", "abcdefg"], ["abef", "xyz"]) (HW2.findDocuments (HW2.None [HW2.None [HW2.Literal "abc"]]) ["abcd", "abef", "xyz", "abcdefg"])
      , "Test 11: HW2.findDocuments (HW2.None [HW2.Literal \"xyz\", HW2.Literal \"mn\"]) [\"abcd\", \"abef\", \"xyz\", \"abcdefg\", \"mnt\"]" ~: testWithTimeout (1000000) (["abcd", "abef", "abcdefg"], ["xyz", "mnt"]) (HW2.findDocuments (HW2.None [HW2.Literal "xyz", HW2.Literal "mn"]) ["abcd", "abef", "xyz", "abcdefg", "mnt"])
      , "Test 12: HW2.findDocuments (HW2.None [HW2.Literal \"xyz\", HW2.Literal \"def\"]) [\"adef\", \"xyz\", \"abcdefg\"]" ~: testWithTimeout (1000000) ([], ["adef", "xyz", "abcdefg"]) (HW2.findDocuments (HW2.None [HW2.Literal "xyz", HW2.Literal "def"]) ["adef", "xyz", "abcdefg"])
      , "Test 13: HW2.findDocuments (HW2.Any [HW2.None [HW2.Literal \"x\"], HW2.All [HW2.Literal \"ab\", HW2.Literal \"yz\"]]) [\"abcd\", \"abef\", \"xyz\", \"abcdefg\", \"abyz\"]" ~: testWithTimeout (1000000) (["abcd", "abef", "abcdefg", "abyz"], ["xyz"]) (HW2.findDocuments (HW2.Any [HW2.None [HW2.Literal "x"], HW2.All [HW2.Literal "ab", HW2.Literal "yz"]]) ["abcd", "abef", "xyz", "abcdefg", "abyz"])
      , "Test 14: HW2.findDocuments (HW2.Any [HW2.All [HW2.Literal \"a\", HW2.None [HW2.Literal \"b\"]], HW2.None [HW2.Literal \"c\"]]) [\"abcd\", \"abef\", \"xyz\", \"acdefg\"]" ~: testWithTimeout (1000000) (["abef", "xyz", "acdefg"], ["abcd"]) (HW2.findDocuments (HW2.Any [HW2.All [HW2.Literal "a", HW2.None [HW2.Literal "b"]], HW2.None [HW2.Literal "c"]]) ["abcd", "abef", "xyz", "acdefg"])
      ]


testIrepeat :: Test
testIrepeat =
  TestList
    [ "Test 1: irepeat 1" ~: testWithTimeout (3 * 1000000) [1,1,1,1,1,1,1,1,1,1] (sample $ HW2.irepeat 1)
    , "Test 2: irepeat 'a'" ~: testWithTimeout (3 * 1000000) "aaaaaaaaaa" (sample $ HW2.irepeat 'a')
    , "Test 3: irepeat True" ~: testWithTimeout (3 * 1000000)  [True,True,True,True,True,True,True,True,True,True] (sample $ HW2.irepeat True)
    ]

testTimeExpired time expected action = do
  result <- timeout time (evaluate $ action)
  case result of
    Just _ -> assertFailure "Expected timeout, but action completed"
    Nothing -> assertEqual "" expected expected




testIcycle :: Test
testIcycle =
  TestList
    [ "Test 1: icycle [1, 2, 3]" ~: testWithTimeout (3 * 1000000) [1,2,3,1,2,3,1,2,3,1] (sample $ HW2.icycle [1, 2, 3])
    , "Test 2: icycle \"abc\"" ~: testWithTimeout (3 * 1000000) "abcabcabca" (sample $ HW2.icycle "abc")
    , "Test 3: icycle [True, False]" ~: testWithTimeout (3 * 1000000)  [True,False,True,False,True,False,True,False,True,False] (sample $ HW2.icycle [True, False])
    ]

testIiterate :: Test
testIiterate =
  TestList
    [ "Test 1: iiterate (\\x -> x * 2) 1" ~: testWithTimeout (3 * 1000000) [2,4,8,16,32,64,128,256,512,1024] (sample $ HW2.iiterate (\x -> x * 2) 1)
    , "Test 2: iiterate (\\x -> x + 1) 0" ~: testWithTimeout (3 * 1000000) [1,2,3,4,5,6,7,8,9,10] (sample $ HW2.iiterate (\x -> x + 1) 0)
    , "Test 3: iiterate (\\x -> x * x) 2" ~: testWithTimeout (3 * 1000000) [4,16,256,65536,4294967296,18446744073709551616,340282366920938463463374607431768211456,115792089237316195423570985008687907853269984665640564039457584007913129639936,13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084096,179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216] (sample $ HW2.iiterate (\x -> x * x) 2)
    ]


testImap :: Test
testImap =
  TestList
    [ "Test 1: imap (* 3) integers" ~: testWithTimeout (3 * 1000000) [0, 3, -3, 6, -6, 9, -9, 12, -12, 15] (sample $ HW2.imap (* 3) HW2.integers)
    , "Test 2: imap (\\x -> x * x) naturals" ~: testWithTimeout (3 * 1000000) [0, 1, 4, 9, 16, 25, 36, 49, 64, 81] (sample $ HW2.imap (\x -> x * x) HW2.naturals)
    , "Test 3: imap (\\x -> x + 1) (irepeat 0)" ~: testWithTimeout (3 * 1000000) [1, 1, 1, 1, 1, 1, 1, 1, 1, 1] (sample $ HW2.imap (\x -> x + 1) (HW2.irepeat 0))
    ]

testIscan :: Test
testIscan =
  TestList
    [ "Test 1: iscan (+) 0 naturals" ~: testWithTimeout (3 * 1000000) [0, 1, 3, 6, 10, 15, 21, 28, 36, 45] (sample $ HW2.iscan (+) 0 HW2.naturals)
    , "Test 2: iscan (*) 1 (irepeat 2)" ~: testWithTimeout (3 * 1000000) [2,4,8,16,32,64,128,256,512,1024] (sample $ HW2.iscan (*) 1 (HW2.irepeat 2))
    , "Test 3: iscan (\\x y -> x + y - 1) 0 (irepeat 3)" ~: testWithTimeout (3 * 1000000) [2,4,6,8,10,12,14,16,18,20] (sample $ HW2.iscan (\x y -> x + y - 1) 0 (HW2.irepeat 3))
    ]

testIzip :: Test
testIzip =
  TestList
    [ "Test 1: izip l1 l2" ~: testWithTimeout (3 * 1000000) [(0,0), (3,5), (-3,10), (6,15), (-6,20), (9,25), (-9,30), (12,35), (-12,40), (15,45)] (sample $ HW2.izip l1 l2)
    , "Test 2: izip l2 l1" ~: testWithTimeout (3 * 1000000) [(0,"a"), (5,"aa"), (10,"aaa"), (15,"aaaa"), (20,"aaaaa"), (25,"aaaaaa"), (30,"aaaaaaa"), (35,"aaaaaaaa"), (40,"aaaaaaaaa"), (45,"aaaaaaaaaa")] (sample $ HW2.izip l2 as)
    ]
  where
    l1 = HW2.imap (*3) HW2.integers
    l2 = HW2.imap (*5) HW2.naturals
    as = HW2.iiterate (\acc -> acc ++ "a") ""

testInterleave :: Test
testInterleave =
  TestList
    [ "Test 1: interleave l1 l2" ~: testWithTimeout (3 * 1000000) [0,0,3,5,-3,10,6,15,-6,20] (sample $ HW2.interleave l1 l2)
    , "Test 2: interleave l2 l1" ~: testWithTimeout (3 * 1000000) [0,0,5,3,10,-3,15,6,20,-6] (sample $ HW2.interleave l2 l1)
    ]
  where
    l1 = HW2.imap (*3) HW2.integers
    l2 = HW2.imap (*5) HW2.naturals


testInits :: Test
testInits =
  TestList
    [ "Test 1: inits naturals" ~: testWithTimeout (3 * 1000000) [[], [0], [0,1], [0,1,2], [0,1,2,3]] (smallSample (HW2.iinits HW2.naturals))
    , "Test 2: inits of infinite list" ~: testWithTimeout (3 * 1000000)  ["","a","aa","aaa","aaaa"] (smallSample (HW2.iinits (HW2.irepeat 'a')))
    , "Test 3: inits of infinite constant list" ~: testWithTimeout (3 * 1000000) [[], [True], [True, True], [True, True, True], [True, True, True, True]] (smallSample (HW2.iinits (HW2.irepeat True)))
    ]

testTails :: Test
testTails =
  TestList
    [ "Test 1: tails naturals" ~: testWithTimeout (3 * 1000000) [[1,2,3,4,5], [2,3,4,5,6], [3,4,5,6,7], [4,5,6,7,8], [5,6,7,8,9]] (smallSample (HW2.imap smallSample (HW2.itails HW2.naturals)))
    , "Test 2: tails of infinite list" ~: testWithTimeout (3 * 1000000) ["aaaaa","aaaaa","aaaaa","aaaaa","aaaaa"] (smallSample (HW2.imap smallSample (HW2.itails (HW2.irepeat 'a'))))
    , "Test 3: tails of infinite constant list" ~: testWithTimeout (3 * 1000000) [[True,True,True,True,True],[True,True,True,True,True],[True,True,True,True,True],[True,True,True,True,True],[True,True,True,True,True]] (smallSample (HW2.imap smallSample (HW2.itails (HW2.irepeat True))))
    ]


testIfind :: Test
testIfind =
  "Test ifind" ~:
    TestList
      [ "Test 1: HW2.ifind (\\x -> x > 8) repeats" ~: testWithTimeout (3 * 1000000) True (HW2.ifind (\x -> x > 8) repeats)
      , "Test 2: HW2.ifind (\\x -> x == (-3)) repeats" ~: testTimeExpired (3 * 1000000) False (HW2.ifind (\x -> x == (-3)) repeats)
      , "Test 3: HW2.ifind (\\x -> x `mod` 390625 == 0) repeats" ~: testWithTimeout (3 * 1000000) True (HW2.ifind (\x -> x == 390625) repeats)
      ]
    where
        repeats = HW2.imap (HW2.iiterate (\x -> x * x)) HW2.naturals


testTrees :: Test
testTrees =
    "Test Trees" ~:
    TestList
    [
        "Test 1: HW2.preOrder tree1" ~: testWithTimeout (3 * 1000000) [1,2,5,6,8,4,7,3] (HW2.preOrder tree1),
        "Test 2: HW2.preOrder tree2" ~: testWithTimeout (3 * 1000000) [3,5,7,9,12,10,6,8,2,4,11] (HW2.preOrder tree2),
        "Test 3: HW2.inOrder tree1" ~: testWithTimeout (3 * 1000000) [5,2,8,6,1,7,3,4] (HW2.inOrder tree1),
        "Test 4: HW2.inOrder tree2" ~: testWithTimeout (3 * 1000000) [9,7,12,5,6,10,3,2,8,11,4] (HW2.inOrder tree2),
        "Test 5: HW2.postOrder tree1" ~: testWithTimeout (3 * 1000000) [5,8,6,2,3,7,4,1] (HW2.postOrder tree1),
        "Test 6: HW2.postOrder tree2" ~: testWithTimeout (3 * 1000000) [9,12,7,6,10,5,2,11,4,8,3] (HW2.postOrder tree2)
    ]
    where
        tree1 =
            HW2.Tree
                (HW2.Tree (HW2.Tree HW2.EmptyTree 5 HW2.EmptyTree) 2 (HW2.Tree (HW2.Tree HW2.EmptyTree 8 HW2.EmptyTree) 6 HW2.EmptyTree))
                1
                (HW2.Tree (HW2.Tree HW2.EmptyTree 7 (HW2.Tree HW2.EmptyTree 3 HW2.EmptyTree)) 4 HW2.EmptyTree)
        tree2 =
            HW2.Tree
                (HW2.Tree (HW2.Tree (HW2.Tree HW2.EmptyTree 9 HW2.EmptyTree) 7 (HW2.Tree HW2.EmptyTree 12 HW2.EmptyTree)) 5 (HW2.Tree (HW2.Tree HW2.EmptyTree 6 HW2.EmptyTree) 10 HW2.EmptyTree))
                3
                (HW2.Tree (HW2.Tree HW2.EmptyTree 2 HW2.EmptyTree) 8 (HW2.Tree (HW2.Tree HW2.EmptyTree 11 HW2.EmptyTree) 4 HW2.EmptyTree))



testLevelOrder :: Test
testLevelOrder =
    TestList
    [
        "Test 1: HW2.levelOrder tree1" ~: testWithTimeout (3 * 1000000) [1,2,4,5,6,7,8,3] (HW2.levelOrder tree1),
        "Test 2: HW2.levelOrder tree2" ~: testWithTimeout (3 * 1000000) [3,5,8,7,10,2,4,9,12,6,11] (HW2.levelOrder tree2)
    ]
    where
        tree1 =
            HW2.Tree
                (HW2.Tree (HW2.Tree HW2.EmptyTree 5 HW2.EmptyTree) 2 (HW2.Tree (HW2.Tree HW2.EmptyTree 8 HW2.EmptyTree) 6 HW2.EmptyTree))
                1
                (HW2.Tree (HW2.Tree HW2.EmptyTree 7 (HW2.Tree HW2.EmptyTree 3 HW2.EmptyTree)) 4 HW2.EmptyTree)
        tree2 =
            HW2.Tree
                (HW2.Tree (HW2.Tree (HW2.Tree HW2.EmptyTree 9 HW2.EmptyTree) 7 (HW2.Tree HW2.EmptyTree 12 HW2.EmptyTree)) 5 (HW2.Tree (HW2.Tree HW2.EmptyTree 6 HW2.EmptyTree) 10 HW2.EmptyTree))
                3
                (HW2.Tree (HW2.Tree HW2.EmptyTree 2 HW2.EmptyTree) 8 (HW2.Tree (HW2.Tree HW2.EmptyTree 11 HW2.EmptyTree) 4 HW2.EmptyTree))



testFromListLevelOrder  :: Test
testFromListLevelOrder =
    TestList
    [
        "Test 1: HW2.levelOrder $ HW2.fromListLevelOrder [1, 2, 3, 4, 5, 6, 7, 8]" ~: testWithTimeout (3 * 1000000) [1, 2, 3, 4, 5, 6, 7, 8] (HW2.levelOrder $ HW2.fromListLevelOrder [1, 2, 3, 4, 5, 6, 7, 8]),
        "Test 2: HW2.levelOrder $ HW2.fromListLevelOrder [3, 5, 8, 2, 7, 9, 6, 10, 4, 12, 11]" ~: testWithTimeout (3 * 1000000) [3, 5, 8, 2, 7, 9, 6, 10, 4, 12, 11] (HW2.levelOrder $ HW2.fromListLevelOrder [3, 5, 8, 2, 7, 9, 6, 10, 4, 12, 11]),
        "Test 3: HW2.levelOrder $ HW2.fromListLevelOrder [1]" ~: testWithTimeout (3 * 1000000) [1] (HW2.levelOrder $ HW2.fromListLevelOrder [1])
    ]


allTests :: Test
allTests =
  test
    [ 
        "testIsPrefixOf" ~: testIsPrefixOf,
        "testIsSuffixOf" ~: testIsSuffixOf,
        "testIsInfixOf" ~: testIsInfixOf,
        "testIsSubseuenceOf" ~: testIsSubseuenceOf,
        "testFindDocuments" ~: testFindDocuments,
        "testIrepeat" ~: testIrepeat,
        "testIcycle" ~: testIcycle,
        "testIiterate" ~: testIiterate,
        "testImap" ~: testImap,
        "testIscan" ~: testIscan,
        "testIzip" ~: testIzip,
        "testInterleave" ~: testInterleave,
        "testInits" ~: testInits,
        "testTails" ~: testTails,
        "testIfind" ~: testIfind,
        "testTrees" ~: testTrees,
        "testLevelOrder" ~: testLevelOrder,
        "testFromListLevelOrder" ~: testFromListLevelOrder

    ] 

-- Run the tests and output the results to a file
main :: IO ()
main = do
  let outputFile = "test_results.txt"
  (counts, failures) <- runTestText (putTextToHandle stdout False) allTests
  putStrLn $ ""
