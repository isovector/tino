module Main where

import           Data.Monoid ((<>))
import           Graphics.X11.ExtraTypes.XF86
import           Log
import           XMonad
import           XMonad.Actions.CopyWindow (copyToAll)
import           XMonad.Actions.WindowGo (raiseMaybe)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import           XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Layout.Accordion
import           XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spiral
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys, removeKeys, additionalMouseBindings, removeMouseBindings)
import           XMonad.Util.Run (safeSpawnProg, safeSpawn, spawnPipe, hPutStrLn)
import           XMonad.Util.WindowProperties (getProp32s)



myManageHook = composeAll
    [ resource  =? "desktop_window" --> doIgnore
    , className =? "stalonetray"    --> doIgnore
    , className =? "xmobar"         --> doIgnore
    , className =? "vlc"            --> doFloat
    , isFullscreen                  --> doFullFloat
    ]



myLayout =
    avoidStruts
    ( Tall 1 (3/100) (1/2)
  ||| ThreeColMid 1 (3/100) (1/2)
  ||| Accordion
  ||| Mirror (Tall 1 (3/100) (1/2))
  ||| Full
  ||| spiral (6/7)
    )
  ||| noBorders (fullscreenFull Full)



runOrRaise :: String -> [String] -> Query Bool -> X ()
runOrRaise = (raiseMaybe .) . safeSpawn

alt  = mod1Mask
modk = mod4Mask

keysToUnbind =
  [ (modk, xK_p)
  , (modk .|. shiftMask, xK_p)
  , (modk, xK_e)
  , (modk, xK_r)
  , (modk, xK_h)
  , (modk, xK_l)
  ]

safeSpawn' p = safeSpawn p . words

-- Color of current window title in xmobar.
xmobarTitleColor = "#daa520"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#daa520"

keysToBind =
  [ ((modk, xK_f),                  runOrRaise "firefox" ["--force-device-scale-factor=0.5"] $ className =? "Firefox")
  , ((modk, xK_g),                  runOrRaise "gvim" [] $ className =? "Gvim")
  , ((modk, xK_d),                  safeSpawnProg "synapse")
  , ((modk, xK_x),                  safeSpawnProg "xfce4-terminal")
  , ((modk, xK_t),                  safeSpawnProg "thunar")
  , ((modk .|. shiftMask, xK_q),    kill)
  , ((modk, xK_p),                  safeSpawnProg "scrot")
  , ((modk .|. shiftMask, xK_p),    spawn "sleep 0.2; scrot -s")
  , ((0, xF86XK_AudioRaiseVolume),  safeSpawn' "amixer" "-c 0 -q set Master 2dB+")
  , ((0, xF86XK_AudioLowerVolume),  safeSpawn' "amixer" "-c 0 -q set Master 2dB-")
  , ((0, xF86XK_MonBrightnessDown), safeSpawn' "xbacklight" "-dec 15")
  , ((0, xF86XK_MonBrightnessUp),   safeSpawn' "xbacklight" "-inc 15")
  , ((modk .|. shiftMask, xK_h),    sendMessage Shrink)
  , ((modk .|. shiftMask, xK_l),    sendMessage Expand)
  , ((modk, xK_F11),                safeSpawn' "redshift" "-x")
  , ((modk, xK_F12),                safeSpawn' "redshift" "-O1500")
  , ((modk .|. controlMask, xK_l),  safeSpawn' "dm-tool" "lock")
  , ((modk .|. controlMask, xK_f),  withFocused $ windows . W.sink)
  ]

buttonsToUnbind =
  [ (modk, button1)
  , (modk, button2)
  , (modk, button3)
  ]

buttonsToBind =
  [ ((alt, button1),  \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , ((alt, button2),  windows . (W.shiftMaster .) . W.focusWindow)
  , ((alt, button3),  \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  , ((modk, button3), \w -> focus w >> withFocused (windows . W.sink))
  ]

main = do
  setupLogger DEBUG "/home/sandy"
  spawn "xmodmap ~/.xmodmaprc"
  spawn "/usr/lib/xfce4/notifyd/xfce4-notifyd"
  spawn "feh --bg-fill wp.jpg"
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"


  xmonad $ docks def
    { borderWidth        = 2
    , terminal           = "xfce4-terminal"
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#770077"
    , modMask = modk
    , logHook = dynamicLogWithPP $
        xmobarPP
        { ppOutput  = hPutStrLn xmproc
        , ppTitle   = xmobarColor xmobarTitleColor "" . shorten 100
        , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
        , ppSep     = "   "
        }
    , startupHook = setWMName "LG3D"
    , layoutHook  = smartBorders myLayout
    , manageHook  = mconcat [ manageDocks
                            , myManageHook
                            , manageHook def
                            ]
    , handleEventHook = fullscreenEventHook
    } `removeKeys`              keysToUnbind
      `additionalKeys`          keysToBind
      `removeMouseBindings`     buttonsToUnbind
      `additionalMouseBindings` buttonsToBind

