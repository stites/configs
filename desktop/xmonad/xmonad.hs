{-# LANGUAGE TypeOperators #-}

import Data.Monoid ((<>))
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToPrev, shiftToNext)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog (xmobar, PP(..))
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.FadeWindows -- (fadeWindowsLogHook)
import XMonad.Hooks.ManageDocks (AvoidStruts, ToggleStruts(..), avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.SetWMName
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Maximize (Maximize, maximize, maximizeRestore)
import XMonad.ManageHook
import XMonad.Prompt (XPrompt, XPConfig(..))
import XMonad.Util.Cursor (setDefaultCursor)

import qualified XMonad.Hooks.DynamicLog as DLog
import qualified XMonad.Hooks.SetWMName as Window
import qualified XMonad.Layout.BinarySpacePartition as BSP
import qualified XMonad.Layout.IndependentScreens as LIS
import qualified XMonad.Layout.Spacing as Spacing
import qualified XMonad.Layout.WindowNavigation as Window (Navigate(..))
import qualified XMonad.Prompt as Prompt
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Run as Run (safeSpawn, spawnPipe)
import qualified XMonad.Util.EZConfig as EZ

-- import System.Taffybar.Hooks.PagerHints (pagerHints)

main :: IO ()
main = do
  -- xmproc <- spawnPipe "xmobar -d ~/git/configs/desktop/xmonad/xmobar.hs"
  -- xmproc <- Run.spawnPipe "xmobar ~/.xmonad/xmobarrc.hs"
  -- xmonad =<< xmobar myConfig
  xmonad $ myConfig
    -- gives taffybar logger information
    -- ewmh $
    -- pagerHints $
    -- xmonad $ defaultConfig { startupHook = setWMName "LG3D" }

myConfig = desktopConfig
  { modMask    = mod4Mask  -- Rebind Mod to super
  , terminal   = "urxvt"
  , workspaces = ["1", "2", "3", "4"]
  , borderWidth        = 1
  , focusFollowsMouse  = False
  -- , manageHook         = manageDocks <+> manageHook def <+> (resource =? launcher --> doIgnore)
  , layoutHook         = layout_hook
  , handleEventHook    = docksEventHook <+> handleEventHook def
  -- , logHook = do
  --     -- fadeWindowsLogHook (composeAll [isUnfocused --> transparency 1.0, opaque]) -- This doesn't seem to do anything
  --     DLog.dynamicLogWithPP DLog.xmobarPP
  --       { ppCurrent = DLog.xmobarColor "black" "gray"
  --       , ppHidden  = DLog.xmobarColor "orange" ""
  --       , ppHiddenNoWindows = id
  --       --, ppOutput  = hPutStrLn xmproc
  --       , ppSep     = DLog.xmobarColor "orange" "" " | "
  --       , ppTitle   = DLog.xmobarColor "lightblue" "" . DLog.shorten 120
  --       , ppOrder   = \[a,_,b] -> [a, b]    -- Don't log layout name
  --       }
  -- , startupHook = startup_hook
  } `EZ.removeKeysP` removeKeys'
    `EZ.additionalKeysP` additionalKeys'

launcher :: String
launcher = "albert"

type (:+) f g = Choose f g
infixr 5 :+

-- Frustrating, but
-- layout_hook
--   :: ModifiedLayout AvoidStruts
--        (ModifiedLayout Maximize
--          (ModifiedLayout SmartSpacing (BSP.BinarySpacePartition :+ Full)))
--      Window
layout_hook = modify (emptyBSP ||| Full)
 where
  modify = avoidStruts . maximize . Spacing.smartSpacing 0
  tall = Tall 1 (3/100) (1/2)


-- startup_hook :: X ()
-- startup_hook = do
--   Window.setWMName "LG3D"
--   setDefaultCursor xC_top_left_arrow
--   -- toggleHDMI
--   where
--     toggleHDMI :: MonadIO m => m ()
--     toggleHDMI = LIS.countScreens >>= spawn . xrandrToggle
--       where
--         xrandrToggle :: Int -> String
--         xrandrToggle sc =
--           case compare sc 1 of
--             GT -> "echo \"foo\" && xrandr"
--             _  -> "echo \"bar\" && xrandr"


removeKeys' :: [String]
removeKeys' = [ "M-S-<Return>" -- terminal
              , "M-S-c"        -- kill
              , "M-<Tab>"      -- focus down
              , "M-S-<Tab>"    -- focus up
              --, "M-<Space>"    -- rebind in additional keys
              , "M-h"          -- shrink
              , "M-l"          -- expand
              , "M-<Return>"   -- swap master
              , "M-m"          -- focus master
              ]


xpconfig :: XPConfig
xpconfig = Prompt.greenXPConfig
  { font = "-misc-fixed-*-*-*-*-20-*-*-*-*-*-*-*"
  , height = 26
  , historySize = 0
  , promptBorderWidth = 0
  }


additionalKeys' :: [(String, X ())]
additionalKeys'
  = windowsAndWorkspace
  <> applications
  <> system
  <> binaryPartitionLayout
  -- <> bspLayoutKeys
  where
    windowsAndWorkspace =
      [ ("M-S-w",   kill)
      , ("M-l",   sendMessage $ Window.Go R)
      , ("M-h",   sendMessage $ Window.Go L)
      , ("M-S-c", removeEmptyWorkspace)
      -- , ("M-S-<Return>", myAddWorkspacePrompt xpconfig)
      , ("M-S-f", withFocused (sendMessage . maximizeRestore))
      -- , ("M-S-<Space>",  sendMessage ToggleLayout)
      -- , ("M-M1-h",       sendMessage Shrink)
      -- , ("M-M1-l",       sendMessage Expand)
      ]

    applications =
      [ ("M-o d",      spawn "thunar")
      , ("M-o h",      promptSearch xpconfig hackage)
      , ("M-<Return>", spawn =<< asks (terminal . config))
      , ("C-S-<Space>",  spawn "albert show")
      -- , ("M-i",          spawn "google-chrome-stable")
      ]

    system =
      [ ("M-S-<Delete>", spawn "pm-hibernate")
      , ("M-S-l", spawn "xfce4-session-logout")
      , ("C-S-<F3>", spawn "amixer -q sset Master toggle")
      , ("C-S-<F5>", spawn "amixer -q sset Master 3%-")
      , ("C-S-<F6>", spawn "amixer -q sset Master 3%+")
      , ("C-S-<F8>", spawn "xbacklight -dec 10")
      , ("C-S-<F9>", spawn "xbacklight -inc 10")
      , ("C-S-<F12>", spawn "xscreensaver-command -lock")
      , ("M-b", sendMessage ToggleStruts)
      ]

    binaryPartitionLayout =
      [ ("M-S-<Left>",    sendMessage $ BSP.ExpandTowards L)
      , ("M-S-<Right>",   sendMessage $ BSP.ExpandTowards R)
      , ("M-S-<Up>",      sendMessage $ BSP.ExpandTowards U)
      , ("M-S-<Down>",    sendMessage $ BSP.ExpandTowards D)

      , ("M-S-h",         sendMessage $ BSP.ExpandTowards L)
      , ("M-S-l",         sendMessage $ BSP.ExpandTowards R)
      , ("M-S-k",         sendMessage $ BSP.ExpandTowards U)
      , ("M-S-j",         sendMessage $ BSP.ExpandTowards D)

      , ("M-s",           sendMessage   BSP.Swap)
      , ("M-S-s",         sendMessage   Rotate)
      , ("M-S-p",         sendMessage   FocusParent)
      ]

-- Like promptSearchBrowser, but open it up so I have access to the flags to
-- pass to the browser. This lets me pass "--new-window" to chrome, so my
-- searches don't appear in new tabs on some random existing browser window.
promptSearchBrowser' :: XPConfig -> Browser -> SearchEngine -> X ()
promptSearchBrowser' config browser (SearchEngine name site) =
    Prompt.mkXPrompt (Search' name) config (Prompt.historyCompletionP ("Search [" `isPrefixOf`))
      (\query -> Run.safeSpawn browser ["--new-window", site query])

newtype Search' = Search' Name

instance XPrompt Search' where
    showXPrompt (Search' name)= "Search [" ++ name ++ "]: "
    nextCompletion _ = Prompt.getNextCompletion
    commandToComplete _ c = c

myManagementHooks :: [ManageHook]
myManagementHooks = [
  resource =? launcher --> doIgnore
  ]

