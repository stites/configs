-- from:
-- https://wiki.archlinux.org/index.php/Xmobar (https://perma.cc/2SHH-4A2Y)
Config {
   -- appearance
     font        = "xft:FiraCode:pixelsize=14:bold:antialias=true"
   , bgColor     = "black"
   , fgColor     = "#646464"
   , position    = Top
   , border      = BottomB
   , borderColor = "#646464"

   -- layout
   , sepChar  = "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "}{ %KBOS% | %battery% | %multicpu% | %memory% | %dynnetwork% | %date%"

   -- general behavior
   -- , lowerOnStart     = True    -- send to bottom of window stack on start
   -- , hideOnStart      = False   -- start with window unmapped (hidden)
   -- , allDesktops      = True    -- show on all desktops
   -- , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest     = True    -- choose widest display (multi-monitor)
   -- , persistent       = True    -- enable/disable hiding (True = disabled)

   -- plugins
   --   Numbers can be automatically colored according to their value. xmobar
   --   decides color based on a three-tier/two-cutoff system, controlled by
   --   command options:
   --     --Low sets the low cutoff
   --     --High sets the high cutoff
   --
   --     --low sets the color below --Low cutoff
   --     --normal sets the color between --Low and --High cutoffs
   --     --High sets the color above --High cutoff
   --
   --   The --template option controls how the plugin is displayed. Text
   --   color can be set by enclosing in <fc></fc> tags. For more details
   --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
   , commands =
       -- weather monitor
       [ Run Weather "KBOS" [ "--template", "<skyCondition> | <fc=#4682B4><tempF></fc>°F"
                            ] 36000

       -- network activity monitor (dynamic interface resolution)
       , Run DynNetwork     [ "--template" , "<dev>: {<tx>,<rx>}kB/s"
                            , "--Low"      , "1000"       -- units: B/s
                            , "--High"     , "5000"       -- units: B/s
                            , "--low"      , "darkgreen"
                            , "--normal"   , "darkorange"
                            , "--high"     , "darkred"
                            ] 100

       -- cpu activity monitor
       , Run MultiCpu       [ "--template" , "Cpu: {<total0>,<total1>,<total2>,<total3>}%"
                            , "--Low"      , "50"         -- units: %
                            , "--High"     , "85"         -- units: %
                            , "--low"      , "darkgreen"
                            , "--normal"   , "darkorange"
                            , "--high"     , "darkred"
                            ] 30

       -- cpu core temperature monitor
       , Run CoreTemp       [ "--template" , "Temp: {<core0>,<core1>,<core2>,<core3>}°C"
                            , "--Low"      , "70"        -- units: °C
                            , "--High"     , "80"        -- units: °C
                            , "--low"      , "darkgreen"
                            , "--normal"   , "darkorange"
                            , "--high"     , "darkred"
                            ] 30

       -- memory usage monitor
       , Run Memory         [ "--template" ,"Mem: <usedratio>%"
                            , "--Low"      , "20"        -- units: %
                            , "--High"     , "90"        -- units: %
                            , "--low"      , "darkgreen"
                            , "--normal"   , "darkorange"
                            , "--high"     , "darkred"
                            ] 30

       -- battery monitor
       , Run Battery        [ "--template" , "Batt: <acstatus>"
                            , "--Low"      , "10"        -- units: %
                            , "--High"     , "80"        -- units: %
                            , "--low"      , "darkred"
                            , "--normal"   , "darkorange"
                            , "--high"     , "darkgreen"

                            , "--" -- battery specific options
                                      -- discharging status
                                      , "-o", "<left>% (<timeleft>)"
                                      -- AC "on" status
                                      , "-O", "<fc=#dAA520>Charging</fc>"
                                      -- charged status
                                      , "-i", "<fc=#006000>Charged</fc>"
                            ] 30

       -- time and date indicator
       -- cool colors: <fc=#ABABAB>
       --   %F = y-m-d date
       --   %a = day of week
       --   %T = h:m:s time
       --   %b = first three of month
       --   %d = day of month
       --   %l = hour (12h)
       --   %M = minutes
       --   %S = seconds
       --   %p = AM/PM
       , Run Date           "<fc=#ABABAB>%F (%a)</fc> <fc=orange>%l:%M %p</fc> " "date" 30

       -- keyboard layout indicator
       , Run Kbd            [ ("us(colemak)", "<fc=grey>Colmk</fc>")
                            , ("us"         , "<fc=orange>US</fc>")
                            ]
       -- , Run Com "apm" ["-l"] "batt_percent" 30
       -- , Run Com "apm" ["-t"] "time_left" 30
     ]
   }
