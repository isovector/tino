module Main where

import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import           XMonad.Actions.CopyWindow (copyToAll)
import           XMonad.Actions.WindowGo (raiseMaybe)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import           XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spiral
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys, removeKeys, additionalMouseBindings, removeMouseBindings)
import           XMonad.Util.Run (safeSpawnProg, safeSpawn, spawnPipe, hPutStrLn)
import           XMonad.Util.WindowProperties (getProp32s)
import           Log



myManageHook = composeAll
    [ resource  =? "desktop_window" --> doIgnore
    , className =? "stalonetray"    --> doIgnore
    , className =? "xmobar"         --> doIgnore
    , className =? "vlc"            --> doFloat
    , kdeOverride                   --> doFloat
    , do
        c <- className
        if c == "zoom"
           then doF copyToAll
           else mempty
    , isFullscreen                  --> doFullFloat
    ]



myLayout = avoidStruts (
    ThreeColMid 1 (3/100) (1/2) |||
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)



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
xmobarTitleColor = "#FFAA44"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#aa33aa"

keysToBind =
  [ ((modk, xK_f),                 runOrRaise "chromium-browser" ["--force-device-scale-factor=0.5"] $ className =? "chromium-browser")
  , ((modk, xK_g),                 runOrRaise "gvim" [] $ className =? "Gvim")
  , ((modk, xK_d),                 safeSpawnProg "synapse")
  , ((modk, xK_x),                 safeSpawnProg "terminator")
  , ((modk, xK_t),                 safeSpawnProg "thunar")
  , ((modk .|. shiftMask, xK_q),   kill)
  , ((modk, xK_p),                 safeSpawnProg "scrot")
  , ((modk .|. shiftMask, xK_p),   spawn "sleep 0.2; scrot -s")
  , ((0, xF86XK_AudioRaiseVolume), safeSpawn' "amixer" "-q set Master 2dB+")
  , ((0, xF86XK_AudioLowerVolume), safeSpawn' "amixer" "-q set Master 2dB-")
  , ((modk .|. shiftMask, xK_h),   sendMessage Shrink)
  , ((modk .|. shiftMask, xK_l),   sendMessage Expand)
  , ((modk, xK_F11),               safeSpawn' "redshift" "-x")
  , ((modk, xK_F12),               safeSpawn' "redshift" "-O2500")
  , ((modk .|. controlMask, xK_l), safeSpawn' "dm-tool" "lock")
  , ((modk .|. controlMask, xK_f), withFocused $ windows . W.sink)
  ]

buttonsToUnbind =
  [ (modk, button1)
  , (modk, button2)
  , (modk, button3)
  ]

buttonsToBind =
  [ ((alt, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , ((alt, button2), windows . (W.shiftMaster .) . W.focusWindow)
  , ((alt, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  ]

kdeOverride = ask >>= \w -> liftX $ do
  override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
  wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
  return $ maybe False (elem $ fromIntegral override) wt

main = do
  setupLogger DEBUG "/home/sandy"
  spawn "xmodmap ~/.xmodmaprc"
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"


  xmonad $ docks def
    { borderWidth        = 2
    , terminal           = "terminator"
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#770077"
    , modMask = modk
    , logHook = dynamicLogWithPP $ xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
      , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
      , ppSep = "   "
      }
    , startupHook = setWMName "LG3D"
    , layoutHook  = smartBorders myLayout
    , manageHook  = manageDocks <+> myManageHook <+> manageHook def
    , handleEventHook = fullscreenEventHook
    } `removeKeys`              keysToUnbind
      `additionalKeys`          keysToBind
      `removeMouseBindings`     buttonsToUnbind
      `additionalMouseBindings` buttonsToBind

