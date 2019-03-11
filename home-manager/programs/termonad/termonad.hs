{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Colour.SRGB (Colour, sRGB24)
import Termonad.App (defaultMain)
import Termonad.Config
  -- ( FontConfig, FontSize(FontSizePoints), Option(Set)
  -- , ShowScrollbar(ShowScrollbarAlways), defaultConfigOptions, defaultFontConfig
  -- , defaultTMConfig, fontConfig, fontFamily, fontSize, options, showScrollbar
  -- )
import Termonad.Config.Colour
  (ColourConfig, addColourExtension, createColourExtension, cursorBgColour
  , defaultColourConfig
  )

-- | This sets the color of the cursor in the terminal.
--
-- This uses the "Data.Colour" module to define a dark-red color.
-- There are many default colors defined in "Data.Colour.Names".
cursBgColor :: Colour Double
cursBgColor = sRGB24 204 0 0

-- | This sets the colors used for the terminal.  We only specify the background
-- color of the cursor.
colConf :: ColourConfig (Colour Double)
colConf =
  defaultColourConfig
    { cursorBgColour = Set cursBgColor
    }

-- | This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    -- { fontFamily = "DejaVu Sans Mono"
    { fontFamily = "FuraCode Nerd Font Mono"
    , fontSize = FontSizePoints 12
    }

main :: IO ()
main = do
  colExt <- createColourExtension colConf
  let termonadConf =
        defaultTMConfig
          { options =
              defaultConfigOptions
                { fontConfig = fontConf
                  -- Make sure the scrollbar is always visible.
                , showScrollbar = ShowScrollbarAlways
                , showMenu = False
                , showTabBar = ShowTabBarIfNeeded
                }
          }
        `addColourExtension` colExt
  defaultMain termonadConf
