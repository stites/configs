{-# LANGUAGE TypeOperators #-}

import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToPrev, shiftToNext)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Maximize (Maximize, maximize, maximizeRestore)
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Util.Cursor
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import qualified XMonad.StackSet as W
import Data.Monoid ((<>))
import qualified XMonad.Layout.IndependentScreens as LIS
import XMonad.Config.Xfce


main :: IO ()
main = do
  --xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
      
  xmonad xfceConfig { modMask = mod4Mask, terminal = "xfce4-terminal" } -- $ --def
      -- { terminal           = "xfce4-terminal"
      -- --, workspaces         = ["1", "2", "3"]
      -- , borderWidth        = 1
      -- --, focusFollowsMouse  = False
      -- , modMask            = mod4Mask -- Rebind Mod to super
      -- --, manageHook         = manageDocks <+> manageHook def <+> (resource =? "synapse" --> doIgnore)
      -- , layoutHook         = layout_hook
      -- , handleEventHook    = docksEventHook <+> handleEventHook def
      -- --, logHook =
      -- --  dynamicLogWithPP xmobarPP
      -- --    { ppCurrent = xmobarColor "black" "gray"
      -- --    , ppHidden  = xmobarColor "orange" ""
      -- --    , ppHiddenNoWindows = id
      -- --    --, ppOutput  = hPutStrLn xmproc
      -- --    , ppSep     = xmobarColor "orange" "" " | "
      -- --    , ppTitle   = xmobarColor "lightblue" "" . shorten 120
      -- --    , ppOrder   = \[a,_,b] -> [a,b]    -- Don't log layout name
      -- --    }
      -- --, startupHook = startup_hook
      -- }
      -- `removeKeysP` removeKeys'
      -- `additionalKeysP` additionalKeys'


type (:+) f g = Choose f g
infixr 5 :+

layout_hook
  :: ModifiedLayout AvoidStruts
       (ModifiedLayout Maximize
         (ModifiedLayout SmartSpacing (Tall :+ Mirror Tall :+ Full)))
     Window
layout_hook = modify (tall ||| Mirror tall ||| Full)
 where
  modify = avoidStruts . maximize . smartSpacing 0
  tall = Tall 1 (3/100) (1/2)


startup_hook :: X ()
startup_hook = do
  setWMName "LG3D"
  setDefaultCursor xC_top_left_arrow
  toggleHDMI
  where
    toggleHDMI :: MonadIO m => m ()
    toggleHDMI = LIS.countScreens >>= spawn . xrandrToggle
      where
        xrandrToggle :: Int -> String
        xrandrToggle sc =
          case compare sc 1 of
            GT        -> "xrandr --output HDMI-2 --auto --primary --mode 3440x1440 --left-of eDP-1"
            otherwise -> "xrandr --output HDMI-2 --off"


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
xpconfig = greenXPConfig
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
  where
    windowsAndWorkspace =
      [ ("M-S-w",   kill)
      , ("M-l",   sendMessage $ Go R)
      , ("M-h",   sendMessage $ Go L)
      , ("M-S-c", removeEmptyWorkspace)
      -- , ("M-S-<Return>", myAddWorkspacePrompt xpconfig)
      , ("M-S-f", withFocused (sendMessage . maximizeRestore))
      -- , ("M-S-<Space>",  sendMessage ToggleLayout)
      , ("M-M1-h",       sendMessage Shrink)
      , ("M-M1-l",       sendMessage Expand)
      ]

    applications =
      [ ("M-o d",      spawn "thunar")
      , ("M-o h",      promptSearch xpconfig hackage)
      , ("M-<Return>", spawn =<< asks (terminal . config))
      , ("C-<Space>",    spawn "synapse")
      -- , ("M-i",          spawn "google-chrome-stable")
      ]

    system =
      [ ("M-S-<Delete>", spawn "sudo pm-hibernate")
      ]

-- Like promptSearchBrowser, but open it up so I have access to the flags to
-- pass to the browser. This lets me pass "--new-window" to chrome, so my
-- searches don't appear in new tabs on some random existing browser window.
promptSearchBrowser' :: XPConfig -> Browser -> SearchEngine -> X ()
promptSearchBrowser' config browser (SearchEngine name site) =
    mkXPrompt (Search' name) config (historyCompletionP ("Search [" `isPrefixOf`))
      (\query -> safeSpawn browser ["--new-window", site query])

data Search' = Search' Name
instance XPrompt Search' where
    showXPrompt (Search' name)= "Search [" ++ name ++ "]: "
    nextCompletion _ = getNextCompletion
    commandToComplete _ c = c

myManagementHooks :: [ManageHook]
myManagementHooks = [
  resource =? "synapse" --> doIgnore
  ]
