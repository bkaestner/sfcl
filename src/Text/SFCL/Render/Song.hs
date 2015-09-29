-- |
-- Module      : Text.SFCL.Render.Song
-- Copyright   : (c) Benjamin Kästner 2015
--
-- Maintainer  : benjamin.kaestner@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides canonical rendering for SFCL.
module Text.SFCL.Render.Song (renderSong) where
import Text.SFCL.Song

-- | Renders a song to the canonical format.
renderSong :: Song -> String
renderSong (Song ls) = unlines $ map renderLine ls

renderLine :: Line -> String
renderLine (Line bs) = unlines $ map renderBlock bs

renderBlock :: Block -> String
renderBlock (CBlock c)    = "@chord{" ++ c ++ "}"
renderBlock (LBlock l)    = l
renderBlock (CLBlock c l) =
  renderBlock (CBlock c) ++ " " ++ renderBlock (LBlock l)