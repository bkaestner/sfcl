{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Text.SFCL
-- Copyright   : (c) Benjamin Kästner 2015
--
-- Maintainer  : benjamin.kaestner@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides parsing according to the SFCL grammar:
--
-- > song        = line, { line ending , line }
-- > line        = block, { block , [line ending]}
-- > block       = {spaces} , (clblock | cblock | lblock)
-- > clblock     = cblock , lblock
-- > cblock      = "@chord{" , chord , "}"
-- > lblock      = lyrics
-- > chord       = [^\r\n\}]+
-- > lyrics      = [^\r\n]+
-- > spaces      = ? US-ASCII 32 ? | ? US-ASCII 9 ? (* spaces and tabs *)
-- > line ending = ["\r"] , "\n"
--
-- While the final grammar is still up to discussion, this should
-- give a good first impression on what a SFCL is about. Also,
-- this is mostly just a prototype to test what's possible with a
-- chord markup language.
--
-- It's unlikely that the final variant will be written in Haskell,
-- unless it's the backend of a translation service to other formats.
module Text.SFCL where
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8

-- | A song is a collection of lines.
newtype Song = Song { getSong :: [Line] } deriving (Eq, Show)

-- | A line is a collection of blocks.
--
-- In a rendering, one would separate different lines with
-- vertical space and have them start on the same horizontal position.
newtype Line = Line { getLine :: [Block] } deriving (Eq, Show)

-- | A block is a chord glued togehter with lyrics.
data Block = CLBlock Chord Lyrics  -- ^ chord and lyrics
           | CBlock Chord          -- ^ only chords (e.g. intro)
           | LBlock Lyrics         -- ^ only lyrics (e.g. no new chord)
  deriving (Eq, Show)

-- | A "chord" in the sense of SFCL could be anything.
-- At the moment, SFCL doesn't put any constraints on the Chord.
-- This might change in a future version, though.
type Chord  = String

-- | Lyrics are text. Simple as that.
type Lyrics = String

-- | Parses a song according to the grammar above.
songParser :: Parser Song
songParser = Song <$> lineParser `sepBy` endOfLine

-- | Parses a line according to the grammar above.
lineParser :: Parser Line
lineParser = Line <$> many1 (blockParser <* optional endOfLine)

-- | Parses a block according to the grammar above.
blockParser :: Parser Block
blockParser = skipSomeSpace *>
            (clblockParser <|> cblockParser <|> lblockParser)

-- | Parses a block that is prefixed by a chord and contains lyrics.
clblockParser :: Parser Block
clblockParser = CLBlock
             <$> (unCBlock <$> cblockParser)
             <*> (unLBlock <$> lblockParser)
  where
    unCBlock (CBlock c) = c
    unLBlock (LBlock l) = l

-- | Parses a block that contains only a chord.
cblockParser :: Parser Block
cblockParser = CBlock . trim <$> p
  where
    p = string "@chord{" *> many1 chordsym <* string "}"
    chordsym = satisfy $ notInClass "@}\r\n"

-- | Parses a block that contains only lyrics.
lblockParser :: Parser Block
lblockParser = LBlock . trim <$> (skipSomeSpace *> many1 lyricsym)
  where
    lyricsym = satisfy $ notInClass "@\r\n"

-- | Helper, which trims whitespace at front and end of a string.
trim :: String -> String
trim = unwords . filter (not . null) . words

-- | Helper, which skips ` ' and `\t', but not line endings.
skipSomeSpace :: Parser ()
skipSomeSpace = skipWhile (`elem` [' ', '\t'])