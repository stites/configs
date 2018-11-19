{-# LANGUAGE OverloadedStrings #-}

import System.Process
import System.Taffybar
import System.Taffybar.Information.Memory
import System.Taffybar.Information.CPU
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingBar
import System.Taffybar.Widget.Generic.PollingGraph

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main :: IO ()
main =
  simpleTaffybar
    $ defaultSimpleTaffyConfig
      { startWidgets = [ workspaces, mpris2 ]
      , centerWidgets = [ currenttime ]
      , endWidgets   = [ currentdate, batt, mem, cpu, tray ]
      }

  where
    memCfg = defaultGraphConfig
      { graphDataColors = [(1, 0, 0, 1)]
      , graphLabel = Just "mem"
      }

    cpuCfg = defaultGraphConfig
      { graphDataColors =
        [ (0, 1, 0, 1)
        , (1, 0, 1, 0.5)
        ]
      , graphLabel = Just "cpu"
      }

    currenttime = textClockNew Nothing "<span fgcolor='orange'>%a %I:%M %p</span>" 1

    currentdate = textClockNew Nothing "<span fgcolor='orange'>%m/%_d </span>" 1

    workspaces = workspacesNew defaultWorkspacesConfig

    mpris2 = mpris2New

    mem = pollingGraphNew memCfg 1 memCallback

    cpu = pollingGraphNew cpuCfg 0.5 cpuCallback

    tray = sniTrayNew

    batt = textBatteryNew "$percentage$ ($time$)"


