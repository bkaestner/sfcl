module Text.SFCL.Render.SongSpec (spec) where
import           Text.SFCL.Render.Song (renderSong)
import           Text.SFCL
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "renderSong" $ do
  context "single lines" $ do
    it "should handle LBlocks" $ property $
      forAll (listOf1 nonCommand) $ \xs ->
        let
          lblock = LBlock xs
          line   = Line [lblock]
          song   = Song [line]
        in renderSong song `shouldBe` xs

    it "should handle multiple LBlocks" $ property $
      forAll (listOf1 $ listOf1 alpha) $ \xs ->
        let
          lblocks = map LBlock xs
          line    = Line lblocks
          song    = Song [line]
        in renderSong song `shouldBe` unwords xs

    it "should handle CBlocks" $ property $
      forAll (listOf1 nonCommand) $ \xs ->
        let
          cblock = CBlock xs
          line   = Line [cblock]
          song   = Song [line]
        in renderSong song `shouldBe` "@chord{" ++ xs ++ "}"

    it "should handle multiple CBlocks" $ property $
      forAll (listOf1 $ listOf1 $ alpha) $ \xs ->
        let
          cblocks = map CBlock xs
          line    = Line cblocks
          song    = Song [line]
          chorded = map (\x -> "@chord{" ++ x ++ "}") xs
        in renderSong song `shouldBe` unwords chorded

    it "should handle CLBlocks" $ pending
    it "should handle multiple CLBlocks" $ pending
    it "should handle mixed lines" $ pending

  context "multiple lines" $
    it "should handle random lines" $ pending

nonCommand :: Gen Char
nonCommand = elements $ filter (`notElem` "@\r\n") [' '..'~']

alpha :: Gen Char
alpha = elements $ ['a'..'z'] ++ ['A'..'Z']