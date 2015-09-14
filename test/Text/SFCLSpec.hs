{-# LANGUAGE OverloadedStrings #-}
module Text.SFCLSpec (spec) where
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