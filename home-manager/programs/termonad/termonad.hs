{-# LANGUAGE OverloadedStrings #-}
{- LANGUAGE NumericUnderscores #-}
module Main where

import Data.Colour.SRGB (Colour, sRGB24)
import Termonad.App (defaultMain)
import Termonad.Config
  -- ( FontConfig, FontSize(FontSizePoints), Option(Set)
  -- , ShowScrollbar(ShowScrollbarAlways), defaultConfigOptions, defaultFontConfig
  -- , defaultTMConfig, fontConfig, fontFamily, fontSize, options, showScrollbar
  -- )
import Termonad.Config.Colour
import Termonad.Config.Vec
import Termonad.Types
import GI.Vte.Objects.Terminal
  -- (ColourConfig, addColourExtension, createColourExtension, cursorBgColour
  -- , defaultColourConfig
  -- )

-- -- | This sets the color of the cursor in the terminal.
-- --
-- -- This uses the "Data.Colour" module to define a dark-red color.
-- -- There are many default colors defined in "Data.Colour.Names".
-- cursBgColor :: Colour Double
-- -- cursBgColor = sRGB24 204 0 0 -- default
-- cursBgColor = sRGB24 100 54 22 -- dot color: cursorColor2
--
-- -- | This sets the colors used for the terminal.  We only specify the background
-- -- color of the cursor.
-- colConf :: ColourConfig (Colour Double)
-- colConf =
--   defaultColourConfig

main :: IO ()
main = do
  colExt <- createColourExtension dotShare -- defaultColourConfig -- solarizedDark solarizedLight
  defaultMain $ TMConfig myOptions (ConfigHooks myHooks) `addColourExtension` colExt
 where
  myOptions :: ConfigOptions
  myOptions = defaultConfigOptions
    { fontConfig = FontConfig
        { fontFamily = "FuraCode Nerd Font Mono"
        , fontSize = FontSizePoints 13
        }
    , showScrollbar = ShowScrollbarIfNeeded
    -- , scrollbackLen = 1_000_000
    , scrollbackLen = 1000000
    , confirmExit = False
    , wordCharExceptions = "-#%&+,./=?@\\_~\183:"
    , showMenu = False
    , showTabBar = ShowTabBarIfNeeded
    , cursorBlinkMode = CursorBlinkModeOff
    }

  myHooks :: TMState -> Terminal -> IO ()
  myHooks s term = pure ()

-- data MultiColourExtension = MultiColourExtension
--   { colourExtConf :: MVar (NonEmpty (ColourConfig (Colour Double)))
--   , colourExtCreateTermHook :: TMState -> Terminal -> IO ()
--   }

-- createMultiColourExtension :: NonEmpty (ColourConfig (Color Double)) -> IO ColourExtension
-- createMultiColourExtension confs = newMVar confs >>= \mvarconf -> do
--   pure $ MultiColourExtension
--     { colourExtConf = mvarConf
--     , colourExtCreateTermHook = colourHook mvarConf
--     }

-- This is our Solarized dark 'ColourConfig'.  It holds all of our dark-related settings.
solarizedDark :: ColourConfig (Colour Double)
solarizedDark =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = sRGB24 131 148 150 -- base0
    -- Set the extended palette that has 2 Vecs of 8 Solarized pallette colours
    , palette = ExtendedPalette solarizedDark1 solarizedDark2
    }
  where
    solarizedDark1 :: Vec N8 (Colour Double)
    solarizedDark1 =
         sRGB24   0  43  54 -- base03, background
      :* sRGB24 220  50  47 -- red
      :* sRGB24 133 153   0 -- green
      :* sRGB24 181 137   0 -- yellow
      :* sRGB24  38 139 210 -- blue
      :* sRGB24 211  54 130 -- magenta
      :* sRGB24  42 161 152 -- cyan
      :* sRGB24 238 232 213 -- base2
      :* EmptyVec

    solarizedDark2 :: Vec N8 (Colour Double)
    solarizedDark2 =
         sRGB24   7  54  66 -- base02, background highlights
      :* sRGB24 203  75  22 -- orange
      :* sRGB24  88 110 117 -- base01, comments / secondary text
      :* sRGB24 131 148 150 -- base0, body text / default code / primary content
      :* sRGB24 147 161 161 -- base1, optional emphasised content
      :* sRGB24 108 113 196 -- violet
      :* sRGB24 101 123 131 -- base00
      :* sRGB24 253 246 227 -- base3
      :* EmptyVec

-- From: http://dotshare.it/dots/87/
dotShare :: ColourConfig (Colour Double)
dotShare =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = sRGB24 84 83 78 -- base0
    -- Set the extended palette that has 2 Vecs of 8 Solarized pallette colours
    , palette = ExtendedPalette colors1 colors2
    }
  where
    colors1 :: Vec N8 (Colour Double)
    colors1 =
         sRGB24   6   6   6 -- base03, background
      :* sRGB24  91  31  31 -- red
      :* sRGB24  72  84  91 -- green
      :* sRGB24  88  67  36 -- yellow
      :* sRGB24  49  76  81 -- blue
      :* sRGB24  61  39  98 -- magenta
      :* sRGB24  43  53  55 -- cyan
      :* sRGB24  87  87  87 -- base2
      :* EmptyVec

    colors2 :: Vec N8 (Colour Double)
    colors2 =
         sRGB24  25  25  25 -- base02, background highlights
      :* sRGB24  82  24  24 -- orange
      :* sRGB24  63  81  36 -- base01, comments / secondary text
      :* sRGB24  93  62  13 -- base0, body text / default code / primary content
      :* sRGB24  32  98  69 -- base1, optional emphasised content
      :* sRGB24  52  26 100 -- violet
      :* sRGB24  26  44  48 -- base00
      :* sRGB24  87  87  87 -- base3
      :* EmptyVec


-- This is our Solarized light 'ColourConfig'.  It holds all of our light-related settings.
solarizedLight :: ColourConfig (Colour Double)
solarizedLight =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = sRGB24 101 123 131 -- base00
    -- Set the extended palette that has 2 Vecs of 8 Solarized pallette colours
    , palette = ExtendedPalette solarizedLight1 solarizedLight2
    }
  where
    solarizedLight1 :: Vec N8 (Colour Double)
    solarizedLight1 =
         sRGB24 238 232 213 -- base2, background highlights
      :* sRGB24 220  50  47 -- red
      :* sRGB24 133 153   0 -- green
      :* sRGB24 181 137   0 -- yellow
      :* sRGB24  38 139 210 -- blue
      :* sRGB24 211  54 130 -- magenta
      :* sRGB24  42 161 152 -- cyan
      :* sRGB24   7  54  66 -- base02
      :* EmptyVec

    solarizedLight2 :: Vec N8 (Colour Double)
    solarizedLight2 =
         sRGB24 253 246 227 -- base3, background
      :* sRGB24 203  75  22 -- orange
      :* sRGB24 147 161 161 -- base1, comments / secondary text
      :* sRGB24 101 123 131 -- base00, body text / default code / primary content
      :* sRGB24  88 110 117 -- base01, optional emphasised content
      :* sRGB24 108 113 196 -- violet
      :* sRGB24 131 148 150 -- base0
      :* sRGB24   0  43  54 -- base03
      :* EmptyVec

