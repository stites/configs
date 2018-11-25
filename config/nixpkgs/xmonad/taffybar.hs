{-# LANGUAGE OverloadedStrings #-}

import System.Process
import System.Taffybar
import System.Taffybar.Information.Memory
import System.Taffybar.Information.CPU
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingBar
import System.Taffybar.Widget.Generic.PollingGraph

main :: IO ()
main =
  simpleTaffybar
    $ defaultSimpleTaffyConfig
      { startWidgets = [ workspaces, mpris2 ]
      , centerWidgets = [ currenttime ]
      , endWidgets   = [ currentdate, batt, mem, cpu, tray ]
      }

  where
    currenttime = textClockNew Nothing "<span fgcolor='orange'>%a %I:%M %p</span>" 1

    currentdate = textClockNew Nothing "<span fgcolor='orange'>%m/%_d </span>" 1

    workspaces = workspacesNew defaultWorkspacesConfig

    mpris2 = mpris2New

    mem = pollingGraphNew memCfg 1 memCallback

    cpu = pollingGraphNew cpuCfg 0.5 cpuCallback

    tray = sniTrayNew

    batt = textBatteryNew "$percentage$ ($time$)"


memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

mkRGBA (r, g, b, a) = (r/256, g/256, b/256, a/256)

yellow1 = mkRGBA (242 , 163 , 54  , 256)
yellow2 = mkRGBA (254 , 204 , 83  , 256)
yellow3 = mkRGBA (227 , 134 , 18  , 256)
blue    = mkRGBA (42  , 99  , 140 , 256)
red     = mkRGBA (210 , 77  , 37  , 256)

myGraphConfig
  = defaultGraphConfig
  { graphPadding         = 0
  , graphBorderWidth     = 0
  , graphWidth           = 75
  , graphBackgroundColor = (0.0, 0.0, 0.0, 0.0)
  }

memCfg = myGraphConfig
  { graphDataColors = [(0.129, 0.588, 0.953, 1)]
  , graphLabel = Just "mem"
  }

cpuCfg = myGraphConfig
  { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
  , graphLabel = Just "cpu"
  }

{-

mkRGBA (r, g, b, a) = (r/256, g/256, b/256, a/256)
blue = mkRGBA (42, 99, 140, 256)
yellow1 = mkRGBA (242, 163, 54, 256)
yellow2 = mkRGBA (254, 204, 83, 256)
yellow3 = mkRGBA (227, 134, 18, 256)
red = mkRGBA (210, 77, 37, 256)

myGraphConfig =
  defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphWidth = 75
  , graphBackgroundColor = (0.0, 0.0, 0.0, 0.0)
  }

netCfg = myGraphConfig
  { graphDataColors = [yellow1, yellow2]
  , graphLabel = Just "net"
  }

memCfg = myGraphConfig
  { graphDataColors = [(0.129, 0.588, 0.953, 1)]
  , graphLabel = Just "mem"
  }

cpuCfg = myGraphConfig
  { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
  , graphLabel = Just "cpu"
  }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

getFullWorkspaceNames :: X11Property [(WorkspaceIdx, String)]
getFullWorkspaceNames = go <$> readAsListOfString Nothing "_NET_DESKTOP_FULL_NAMES"
  where go = zip [WSIdx i | i <- [0..]]

workspaceNamesLabelSetter workspace =
  fromMaybe "" . lookup (workspaceIdx workspace) <$>
    liftX11Def [] getFullWorkspaceNames

enableLogger logger level = do
  logger <- getLogger logger
  saveGlobalLogger $ setLevel level logger

logDebug = do
  logger <- getLogger "System.Taffybar.Widget.Generic.AutoSizeImage"
  saveGlobalLogger $ setLevel DEBUG logger
  logger2 <- getLogger "StatusNotifier.Tray"
  saveGlobalLogger $ setLevel DEBUG logger2
  workspacesLogger <- getLogger "System.Taffybar.Widget.Workspaces"
  saveGlobalLogger $ setLevel WARNING workspacesLogger

-- github = do
--   Right (token, _) <- passGet "github-token"
--   githubNotificationsNew $ defaultGithubConfig $ Auth.OAuth $ BS.pack token

main = getHomeDirectory go

go homeDirectory = do
  -- logDebug
  -- logM "What" WARNING "Why"
  -- enableLogger "System.Taffybar.Widget.Util" DEBUG
  -- enableLogger "System.Taffybar.Information.XDG.DesktopEntry" DEBUG
  -- enableLogger "System.Taffybar.WindowIcon" DEBUG
  startTaffybar $
    appendHook notifySystemD $
    appendHook (void $ getHost False) $
    withBatteryRefresh $
    withLogServer $
    withToggleServer $
    toTaffyConfig simpleTaffyConfig

  where
    resourcesDirectory = homeDirectory </> ".lib" </> "resources"
    inResourcesDirectory file = resourcesDirectory </> file
    highContrastDirectory = "/" </> "usr" </> "share" </> "icons" </> "HighContrast" </> "256x256"
    inHighContrastDirectory file = highContrastDirectory </> file
    getIconFileName w@WindowData {windowTitle = title, windowClass = klass}
        -- | "URxvt" `isInfixOf` klass = Just "urxvt.png"
        -- | "Termite" `isInfixOf` klass = Just "urxvt.png"
        -- | "Kodi" `isInfixOf` klass = Just "kodi.png"
        | "@gmail.com" `isInfixOf` title &&
              "chrome" `isInfixOf` klass &&
               "Gmail" `isInfixOf` title
               = Just "gmail.png"
        | otherwise = Nothing
    myIcons = scaledWindowIconPixbufGetter $
                unscaledDefaultGetWindowIconPixbuf <|||>
                (\size _ -> lift $ loadPixbufByName size "application-default-icon")
    cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
    mem = pollingGraphNew memCfg 1 memCallback
    layout = layoutNew defaultLayoutConfig
    windows = windowsNew defaultWindowsConfig
    notifySystemD = void $ runCommandFromPath ["systemd-notify", "--ready"]
    myWorkspacesConfig =
      defaultWorkspacesConfig
      { underlineHeight = 3
      , underlinePadding = 2
      , minIcons = 1
      , getWindowIconPixbuf = myIcons
      -- , windowIconSize = 31
      , widgetGap = 0
      , showWorkspaceFn = hideEmpty
      , updateRateLimitMicroseconds = 100000
      , labelSetter = workspaceNamesLabelSetter
      }
    workspaces = workspacesNew myWorkspacesConfig
    simpleTaffyConfig = defaultSimpleTaffyConfig
      { startWidgets = workspaces : map (>>= buildContentsBox) [layout, windows]
      , endWidgets = map (>>= buildContentsBox)
        [ textBatteryNew "$percentage$%"
        , batteryIconNew
        , textClockNew Nothing "%a %b %_d %r" 1
        , sniTrayNew
     -- ,ithu
        , cpu
        , mem
        , networkGraphNew netCfg Nothing
         networkMonitorNew defaultNetFormat Nothing >>= setMinWidth 200
     -- ,sMonNew 60 ["/dev/sdd2"]
        , mpris2New
        ]
      , barPosition = Top
      , barPadding = 0
      , barHeight = 45
   -- , endWidgets = []
   -- , startWidgets = [flip widgetSetClass "Workspaces" =<< workspaces]
      }

-}
