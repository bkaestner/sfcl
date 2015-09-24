{-# LANGUAGE OverloadedStrings #-}
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

renderSong :: Song -> String
renderSong (Song ls) = unlines' $ map renderLine ls
  where
    unlines' []       = []
    unlines' [x]      = x
    unlines' (x:y:xs) = x ++ "\n" ++ unlines' (y:xs)

renderLine :: Line -> String
renderLine (Line bs) = unwords $ map renderBlock bs

renderBlock :: Block -> String
renderBlock (CBlock c)    = "@chord{" ++ c ++ "}"
renderBlock (LBlock l)    = l
renderBlock (CLBlock c l) =
  renderBlock (CBlock c) ++ " " ++ renderBlock (LBlock l)