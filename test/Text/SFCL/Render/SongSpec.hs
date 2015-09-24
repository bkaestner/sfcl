module Text.SFCL.Render.SongSpec (spec) where
import           Text.SFCL.Render.Song (renderSong)
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Text.SFCL
import           Test.Hspec
import           Test.Hspec.Attoparsec
import           Test.QuickCheck

spec :: Spec
spec = describe "renderSong" $
  it "should leave LBlocks" $ property $
    forAll (listOf1 nonCommand) $ \xs ->
      let
        lblock = LBlock xs
        line   = Line [lblock]
        song   = Song [line]
      in renderSong song `shouldBe` xs

nonCommand :: Gen Char
nonCommand = elements $ filter (`notElem` "@\r\n") [' '..'~']