module Text.SFCL.Render.SongSpec (spec) where
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Text.SFCL
import           Test.Hspec
import           Test.Hspec.Attoparsec
import           Test.QuickCheck

spec :: Spec
spec = do
  return ()