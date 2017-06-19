module Main where

import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import           XMonad.Actions.WindowGo (raiseMaybe)
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys, removeKeys, additionalMouseBindings, removeMouseBindings)
import           XMonad.Util.Run (safeSpawnProg, safeSpawn)


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

main = do
  spawn "xmodmap ~/.xmodmaprc"

  xmonad $ def
    { borderWidth        = 2
    , terminal           = "terminator"
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , modMask = modk
    } `removeKeys`              keysToUnbind
      `additionalKeys`          keysToBind
      `removeMouseBindings`     buttonsToUnbind
      `additionalMouseBindings` buttonsToBind

