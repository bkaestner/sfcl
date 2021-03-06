module Text.SFCL.Render.SongSpec (spec) where
import Text.SFCL.SongSpec ()
import           Control.Applicative (liftA2)
import qualified Data.ByteString.Char8 as B
import           Text.SFCL.Song
import           Text.SFCL.Render.Song (renderSong)
import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Attoparsec
import qualified Test.Hspec as H
import           Test.QuickCheck

-- The default variant of "shouldBe" uses ==.
-- This version however ignores whitespace.
shouldBe :: String -> String -> Expectation
shouldBe a' b' =
  let
    f = unwords . filter (not . null) . map (unwords . words) . lines
    a = f a'
    b = f b'
  in H.shouldBe a b

infix 1 `shouldBe`

spec :: Spec
spec = describe "renderSong" $ do
  context "single lines" $ do
    it "handles LBlocks" $ property $
      forAll ncString $ \xs ->
        let
          lblock = LBlock xs
          line   = Line [lblock]
          song   = Song [line]
        in renderSong song `shouldBe` xs

    it "handles multiple LBlocks" $ property $
      forAll (listOf1 $ listOf1 alpha) $ \xs ->
        let
          lblocks = map LBlock xs
          line    = Line lblocks
          song    = Song [line]
        in renderSong song `shouldBe` unwords xs

    it "handles CBlocks" $ property $
      forAll ncString $ \xs ->
        let
          cblock = CBlock xs
          line   = Line [cblock]
          song   = Song [line]
        in renderSong song `shouldBe` "@chord{" ++ xs ++ "}"

    it "handles multiple CBlocks" $ property $
      forAll (listOf1 $ listOf1 $ alpha) $ \xs ->
        let
          cblocks = map CBlock xs
          line    = Line cblocks
          song    = Song [line]
          chorded = map (\x -> "@chord{" ++ x ++ "}") xs
        in renderSong song `shouldBe` unwords chorded

    it "handles CLBlocks" $ property $
      forAll ncString $ \chord ->
      forAll ncString $ \lyric ->
        let
          clblock = CLBlock chord lyric
          line   = Line [clblock]
          song   = Song [line]
        in renderSong song `shouldBe` "@chord{" ++ chord ++ "} " ++ lyric

    it "handles multiple CLBlocks" $ property $
      forAll (listOf1 $ liftA2 (,) ncString ncString) $ \xs ->
        let
          clblocks = map (uncurry CLBlock) xs
          line   = Line clblocks
          song   = Song [line]
          result = unlines $ map (\(c,l) -> "@chord{" ++ c ++ "} " ++ l) xs
        in renderSong song `shouldBe` result

    it "should handle mixed lines" $ property $ \line ->
      let
        song = Song [line]
        k = B.pack $ renderSong song
      in k ~> songParser `shouldParse` song

  context "multiple lines" $
    it "should handle random lines" $ property $ \song ->
      let k = B.pack $ renderSong song
      in k ~> songParser `shouldParse` song

nonCommand :: Gen Char
nonCommand = elements $ filter (`notElem` "@\r\n") [' '..'~']

ncString :: Gen String
ncString = listOf1 nonCommand

alpha :: Gen Char
alpha = elements $ ['a'..'z'] ++ ['A'..'Z']