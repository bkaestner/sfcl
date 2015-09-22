{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Arbitrary instances
module Text.SFCL.SongSpec (spec) where
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Text.SFCL
import           Test.Hspec
import           Test.Hspec.Attoparsec
import           Test.QuickCheck

spec :: Spec
spec = describe "songParser" $ do
  it "succeeds on valid songs" $ do
    songParser `shouldSucceedOn` songLines
    songParser `shouldSucceedOn` songFormatted

  it "succeeds on songs that have no chords" $ property $
    forAll (listOf $ elements $ '\t' : filter ('@' /=)[' '..'~']) $ \s ->
      songParser `shouldSucceedOn` B.pack s

  it "parses both versions to the same AST" $ do
    parseOnly songParser songLines `shouldBe` parseOnly songParser songFormatted

  it "parses both versions to the correct AST" $ do
      songFormatted ~> songParser `shouldParse` song
      songLines     ~> songParser `shouldParse` song

  it "provides an identity" $ property $ \s ->
    let k = B.pack $ toFormattedSong s
    in k ~> songParser `shouldParse` s


song :: Song
song = Song
  [ Line
      [ CBlock "Cm"
      , CBlock "Ab"
      , CBlock "Eb"
      , CBlock "Bb"
      ]
  , Line
      [ LBlock "What is"
      , CLBlock "Cm" "up with the chords they"
      , CLBlock "Ab" "don't align anymore"
      ]
  , Line
      [ LBlock "when I"
      , CLBlock "Eb" "copy them into"
      , CLBlock "Bb" "Word?"
      ]
  ]

songLines :: ByteString
songLines = B.unlines
  [ "@chord{Cm} @chord{Ab} @chord{Eb} @chord{Bb}"
  , ""
  , "What is @chord{Cm} up with the chords they @chord{Ab} don't align anymore"
  , ""
  , "when I @chord{Eb} copy them into @chord{Bb} Word?"
  ]

songFormatted :: ByteString
songFormatted = B.unlines
  [ "@chord{Cm}"
  , "@chord{Ab}"
  , "@chord{Eb}"
  , "@chord{Bb}"
  , ""
  , "           What is"
  , "@chord{Cm} up with the chords they"
  , "@chord{Ab} don't align anymore"
  , ""
  , "           when I"
  , "@chord{Eb} copy them into"
  , "@chord{Bb} Word?"
  ]

--------------------------------------------------------------------

instance Arbitrary Song where
  arbitrary = resize 120 $ Song <$> listOf1 arbitrary
  -- shrink (Song xs) = map (Song . pure) xs

instance Arbitrary Line where
  arbitrary = resize 4 $ Line <$> listOf1 arbitrary
  -- shrink (Line xs) = map (Line . pure) xs

instance Arbitrary Block where
  arbitrary = do
    k <- choose (1, 3) :: Gen Int
    let syms    = elements $ ['a'..'z'] ++ ['A'..'Z']
    let trimmed = unwords <$> listOf1 (listOf1 syms)
    let chord   = trimmed
    let lyrics  = trimmed
    case k of
      1 -> CLBlock <$> chord <*> lyrics
      2 -> CBlock  <$> chord
      3 -> LBlock  <$> lyrics

toFormattedSong :: Song -> String
toFormattedSong (Song xs) = unlines . map ln $ xs
  where
    ln (Line ls)     = unlines . map bl $ ls
    bl (CLBlock c l) = "@chord{" ++ c ++ "} " ++ l
    bl (CBlock c)    = "@chord{" ++ c ++ "}"
    bl (LBlock l)    = l