module Units (tests) where

import Control.Applicative

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Binary
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Numeric

import Test.HUnit ((@=?), assertBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Data.RFC1751

tests :: [Test]
tests =
    [ testGroup "RFC-1751 unit tests"
        [ testGroup "Encode keys in RFC-1751"
            . map encodeTest  $ zip [1..] vectors
        , testGroup "Decode keys in RFC-1751"
            . map decodeTest  $ zip [1..] vectors
        , testGroup "Encode keys in RFC-1751 (lowercase)"
            . map encodeTest  $ zip [1..] vectorsLC
        , testGroup "Decode keys in RFC-1751 (lowercase)"
            . map decodeTest  $ zip [1..] vectorsLC
        , testGroup "Bad checksums"
            . map badChecksum $ zip [1..] badChecksumVectors
        , testGroup "Bad checksums (lowercase)"
            . map badChecksum $ zip [1..] badChecksumVectorsLC
        , testGroup "Not in dictionary"
            . map notDict $ zip [1..] notDictVectors
        , testGroup "Not in dictionary (lowercase)"
            . map notDict $ zip [1..] notDictVectorsLC
        ]
    ]

type TestVector = (ByteString, String)

badChecksumVectors :: [String]
badChecksumVectors =
    [ "RASH BUSH MILK LOOK BAD A AVID GAFF BAIT ROT POD LOVE"
    , "TAB BORE DUNK SURE COVE NORM PRY IF JOE MYRA GWEN TENT"
    , "BORN ROLL LOVE BEAR AGEE IFFY CUTS MASK MOOD FOWL ROME MIT"
    ]

badChecksumVectorsLC :: [String]
badChecksumVectorsLC =
    [ "rash bush milk look bad a avid gaff bait rot pod love"
    , "tab bore dunk sure cove norm pry if joe myra gwen tent"
    , "born roll love bear agee iffy cuts mask mood fowl rome mit"
    ]

badChecksum :: (Int, String) -> Test
badChecksum (i, s) = testCase ("Bad checksum #" ++ show i)
    (assertBool "bad checksum" . isNothing $ mnemonicToKey s)

notDictVectors :: [String]
notDictVectors =
    [ "PHON MEMO HOP NELL RET DEAF HURT YAWN FLAG MILE LEO LESK"
    , "JAG SUCH PER HASH FULL PHON DAN THEY CAIN BOND LEFT COCA"
    , "TENT TIER LIEU ROD URGE BOWL PATK HOOK FLEW ELY MAN OAK"
    , "TIE OLDY FEEL DOCK EWE PA EMIT HAVE HIS TOTE SWAN KTUH"
    ]

notDictVectorsLC :: [String]
notDictVectorsLC =
    [ "phon memo hop nell ret deaf hurt yawn flag mile leo lesk"
    , "jag such per hash full phon dan they cain bond left coca"
    , "tent tier lieu rod urge bowl patk hook flew ely man oak"
    , "tie oldy feel dock ewe pa emit have his tote swan ktuh"
    ]

notDict :: (Int, String) -> Test
notDict (i, s) = testCase ("Not in dictionary #" ++ show i)
    (assertBool "wrong word" . isNothing $ mnemonicToKey s)

integerToBS :: Integer -> ByteString
integerToBS 0 = BS.pack [0]
integerToBS i 
    | i > 0     = BS.pack $ reverse $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where 
    f 0 = Nothing
    f x = Just $ (fromInteger x :: Word8, x `shiftR` 8)

hexToBS :: String -> ByteString
hexToBS str
    | null str  = BS.empty
    | otherwise = BS.append z2 r2
  where 
    (z,r) = span (== '0') $ filter (/= ' ') str
    z2    = BS.replicate (fromIntegral $ length z `div` 2) 0
    r1    = readHex r
    r2 | null r    = BS.empty
       | null r1   = error $ "cannot read hex"
       | otherwise = integerToBS $ fst $ head r1

vectors :: [TestVector]
vectors =
  [ ( hexToBS "CCAC 2AED 5910 56BE 4F90 FD44 1C53 4766"
    , "RASH BUSH MILK LOOK BAD BRIM AVID GAFF BAIT ROT POD LOVE"
    )
  , ( hexToBS "EFF8 1F9B FBC6 5350 920C DD74 16DE 8009"
    , "TROD MUTE TAIL WARM CHAR KONG HAAG CITY BORE O TEAL AWL"
    )
  ]

vectorsLC :: [TestVector]
vectorsLC =
  [ ( hexToBS "ccac 2aed 5910 56be 4f90 fd44 1c53 4766"
    , "rash bush milk look bad brim avid gaff bait rot pod love"
    )
  , ( hexToBS "eff8 1f9b fbc6 5350 920c dd74 16de 8009"
    , "trod mute tail warm char kong haag city bore o teal awl"
    )
  ]

encodeTest :: (Int, TestVector) -> Test
encodeTest (i, (bs, m)) = testCase ("Encode #" ++ show i)
    (bs @=? fromMaybe BS.empty (mnemonicToKey m))

decodeTest :: (Int, TestVector) -> Test
decodeTest (i, (bs, hk)) = testCase ("Decode #" ++ show i)
    (map toLower hk @=? fromMaybe "" (map toLower <$> keyToMnemonic bs))

