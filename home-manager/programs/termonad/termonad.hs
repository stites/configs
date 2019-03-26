{-# LANGUAGE OverloadedStrings #-}
{- LANGUAGE NumericUnderscores #-}
module Main where

import Data.Colour.SRGB -- (Colour, sRGB24, sRGB24read)
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
  colExt <- createColourExtension eightiesDark -- defaultColourConfig -- solarizedDark solarizedLight
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
    , showMenu = True
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

ambiance :: ColourConfig (Colour Double)
ambiance =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { cursorFgColour   = Set cursorColor
    , cursorBgColour   = Set cursorColor
    , backgroundColour = sRGB24read "#282828"
    , foregroundColour = sRGB24read "#F2F1F0"
    -- Set the extended palette that has 2 Vecs of 8 Solarized pallette colours
    , palette = ExtendedPalette normalColours brightColours
    }

  where
    cursorColor :: Colour Double
    cursorColor = sRGB24read "#F07746"

    normalColours :: Vec N8 (Colour Double)
    normalColours = asVec8 $ MyPallete
      { _black   = sRGB24read "#222222"
      , _red     = sRGB24read "#E84F4F"
      , _green   = sRGB24read "#B7CE42"
      , _yellow  = sRGB24read "#F07746"
      , _blue    = sRGB24read "#66AABB"
      , _magenta = sRGB24read "#B7416E"
      , _cyan    = sRGB24read "#6D878D"
      , _white   = sRGB24read "#F2F1F0"
      }

    brightColours :: Vec N8 (Colour Double)
    brightColours = asVec8 $ MyPallete
      { _black   = sRGB24read "#666666"
      , _red     = sRGB24read "#D23D3D"
      , _green   = sRGB24read "#BDE077"
      , _yellow  = sRGB24read "#F07746"
      , _blue    = sRGB24read "#AACCBB"
      , _magenta = sRGB24read "#E16A98"
      , _cyan    = sRGB24read "#42717B"
      , _white   = sRGB24read "#F2F1F0"
      }

-- From: https://terminal.sexy (schemes: eighties.dark, export: alacritty, xresources)
eightiesDark :: ColourConfig (Colour Double)
eightiesDark =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { cursorFgColour   = Set fgColor
    , cursorBgColour   = Set fgColor
    , backgroundColour = sRGB24read "#2d2d2d"
    , foregroundColour = fgColor
    -- Set the extended palette that has 2 Vecs of 8 Solarized pallette colours
    , palette = ExtendedPalette normalColours brightColours
    }

  where
    fgColor :: Colour Double
    fgColor = sRGB24read "#d3d0c8"

    normalColours :: Vec N8 (Colour Double)
    normalColours = asVec8 $ MyPallete
      { _black   = sRGB24read "#2d2d2d"
      , _red     = sRGB24read "#f2777a"
      , _green   = sRGB24read "#99cc99"
      , _yellow  = sRGB24read "#ffcc66"
      , _blue    = sRGB24read "#6699cc"
      , _magenta = sRGB24read "#cc99cc"
      , _cyan    = sRGB24read "#66cccc"
      , _white   = sRGB24read "#d3d0c8"
      }

    brightColours :: Vec N8 (Colour Double)
    brightColours = asVec8 $ MyPallete
      { _black   = sRGB24read "#747369"
      , _red     = sRGB24read "#f2777a"
      , _green   = sRGB24read "#99cc99"
      , _yellow  = sRGB24read "#ffcc66"
      , _blue    = sRGB24read "#6699cc"
      , _magenta = sRGB24read "#cc99cc"
      , _cyan    = sRGB24read "#66cccc"
      , _white   = sRGB24read "#f2f0ec"
      }

data MyPallete = MyPallete
  { _black   :: Colour Double
  , _red     :: Colour Double
  , _green   :: Colour Double
  , _yellow  :: Colour Double
  , _blue    :: Colour Double
  , _magenta :: Colour Double
  , _cyan    :: Colour Double
  , _white   :: Colour Double
  }

asVec8 :: MyPallete -> Vec N8 (Colour Double)
asVec8 p
  =  _black p
  :* _red p
  :* _green p
  :* _yellow p
  :* _blue p
  :* _magenta p
  :* _cyan p
  :* _white p
  :* EmptyVec

-- colors:
--   # Default colors
--   primary:
--     background: '0x2d2d2d'
--     foreground: '0xd3d0c8'










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

