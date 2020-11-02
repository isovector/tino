{-# LANGUAGE NumDecimals #-}

module Main where

import XMonad.Layout.BinarySpacePartition
import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import           Data.Foldable
import           Data.Monoid (Endo (..))
import           Graphics.X11.ExtraTypes.XF86
import           System.Directory (setCurrentDirectory)
import           System.IO (hGetContents)
import           System.IO.Capture (capture)
import           XMonad
import           XMonad.Actions.CopyWindow (copyToAll)
import           XMonad.Actions.Search hiding (Query)
import           XMonad.Actions.WindowGo (raiseMaybe)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import           XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen, doSideFloat, Side (SE))
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Layout.Accordion
import           XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.ThreeColumns
import           XMonad.Prompt (greenXPConfig, XPConfig(font))
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys, removeKeys, additionalMouseBindings, removeMouseBindings)
import           XMonad.Util.Run (safeSpawnProg, safeSpawn, spawnPipe, hPutStrLn)
import           XMonad.Util.WindowProperties (getProp32s)


myWorkspaces :: [String]
myWorkspaces =
  [ "www"
  , "work"
  , "side"
  , "read"
  , "5"
  , "comm"
  , "7"
  , "8"
  , "music"
  ]


myManageHook :: Query (Endo WindowSet)
myManageHook = fold
    [ resource  =? "desktop_window" --> doIgnore
    , className =? "stalonetray"    --> doIgnore
    , className =? "anki"           --> doFloat
    , className =? "Anki"           --> doFloat
    , className =? "vlc"            --> doFloat
    , className =? "Spotify"        --> doShift "music"
    , className =? "Signal"         --> doShift "comm"
    , title     =? "New entry"      --> doFloat
    , role      =? "conversation"   --> doSideFloat SE
    , kdeOverride                   --> doFloat
    , do
        c <- className
        if c == "zoom"
           then doF copyToAll
           else mempty
    , isFullscreen                  --> doFullFloat
    ]

myDynamicManageHook :: Query (Endo WindowSet)
myDynamicManageHook = fold
    [ className =? "Spotify" --> doShift "music"
    , className =? "Signal"  --> doShift "comm"
    ]


role :: Query String
role = stringProperty "WM_WINDOW_ROLE"


myLayout =
    avoidStruts
    ( Tall 1 (3/100) (1/2)
  ||| ThreeColMid 1 (3/100) (1/2)
  ||| Accordion
  ||| Mirror (Tall 1 (3/100) (1/2))
  ||| Full
  ||| spiral (6/7)
  ||| emptyBSP
    )
  ||| noBorders (fullscreenFull Full)

runOrRaise :: String -> [String] -> Query Bool -> X ()
runOrRaise = (raiseMaybe .) . safeSpawn

alt :: KeyMask
alt  = mod1Mask

musk :: KeyMask
musk = mod3Mask

modk :: KeyMask
modk = mod4Mask

keysToUnbind :: [(KeyMask, KeySym)]
keysToUnbind =
  [ (modk, xK_p)
  , (modk .|. shiftMask, xK_p)
  , (modk, xK_e)
  , (modk, xK_r)
  , (modk, xK_h)
  , (modk, xK_l)
  ]


safeSpawn' :: MonadIO m => FilePath -> String -> m ()
safeSpawn' p = safeSpawn p . words

keysToBind :: [((KeyMask, KeySym), X ())]
keysToBind =
  [ ((modk, xK_f),                  runOrRaise "firefox" [] $ className =? "firefox")
  , ((modk, xK_g),                  runOrRaise "neovide" [] $ className =? "neovide")
  , ((modk, xK_m),                  runOrRaise "spotify" [] $ className =? "Spotify")
  , ((modk, xK_s),                  runOrRaise "signal-desktop" [] $ className =? "Signal")
  , ((modk, xK_d),                  safeSpawn' "rofi" "-show run")
  -- , ((modk, xK_s),                  safeSpawn' "/home/sandy/.tino/bin/rofi-find" "")
  , ((modk, xK_h),                  safeSpawn' "/home/sandy/.tino/bin/rofi-hackage" "")
  , ((modk, xK_b),                  safeSpawn' "/home/sandy/.tino/bin/rofi-web" "")
  , ((modk, xK_x),                  safeSpawnProg "xfce4-terminal")
  , ((modk, xK_t),                  safeSpawnProg "thunar")
  , ((modk .|. shiftMask, xK_q),    kill)
  , ((modk, xK_p),                  safeSpawnProg "scrot")
  , ((modk .|. shiftMask, xK_p),    spawn "sleep 0.2; scrot -s")
  , ((0, xF86XK_AudioRaiseVolume),  safeSpawn' "amixer" "-c 1 -q set Master 2dB+")
  , ((0, xF86XK_AudioLowerVolume),  safeSpawn' "amixer" "-c 1 -q set Master 2dB-")
  , ((0, xF86XK_MonBrightnessDown), safeSpawn' "xbacklight" "-dec 1")
  , ((0, xF86XK_MonBrightnessUp),   safeSpawn' "xbacklight" "-inc 1")
  , ((shiftMask, xF86XK_MonBrightnessDown), safeSpawn' "xbacklight" "-dec 15")
  , ((shiftMask, xF86XK_MonBrightnessUp),   safeSpawn' "xbacklight" "-inc 15")
  , ((modk .|. shiftMask, xK_h),    sendMessage Shrink)
  , ((modk .|. shiftMask, xK_l),    sendMessage Expand)
  , ((modk, xK_F10), do
        safeSpawn' "xrandr" "--output HDMI1 --mode 1920x1080 --left-of eDP1"
        safeSpawn' "polybar" "example"
    )
  , ((modk, xK_F9),                 safeSpawn' "xrandr" "--output HDMI1 --off")
  , ((modk, xK_F11),                safeSpawn' "redshift" "-x")
  , ((modk, xK_F12),                safeSpawn' "redshift" "-O1500")
  , ((modk .|. controlMask, xK_l),  safeSpawn' "dm-tool" "lock")
  , ((modk .|. controlMask, xK_h),  safeSpawn' "systemctl" "suspend")
  , ((modk .|. controlMask, xK_f),  withFocused $ windows . W.sink)
  , ((musk, xK_Left),               safeSpawn' "playerctl" "previous --player=spotify")
  , ((musk, xK_Right),              safeSpawn' "playerctl" "next --player=spotify")
  , ((musk, xK_Down),               safeSpawn' "playerctl" "play-pause --player=spotify")
  ] ++ fmap (uncurry mkShortcut) shortcuts

mkShortcut :: MonadIO m => KeySym -> String -> ((KeyMask, KeySym), m ())
mkShortcut ks url =
  ((modk .|. alt, ks),          safeSpawn' "xdg-open" $ "https://" <> url)

shortcuts :: [(KeySym, String)]
shortcuts =
  [ (xK_g, "gmail.com")
  , (xK_r, "reddit.com")
  , (xK_d, "feedreader.com/online")
  , (xK_f, "functionalprogramming.slack.com")
  , (xK_y, "youtube.com/feed/subscriptions")
  , (xK_m, "g.page/anytimefitnessvictoriadowntown")
  ]

buttonsToUnbind :: [(KeyMask, Button)]
buttonsToUnbind =
  [ (modk, button1)
  , (modk, button2)
  , (modk, button3)
  ]

buttonsToBind :: [((KeyMask, Button), Window -> X ())]
buttonsToBind =
  [ ((alt, button1),  \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , ((alt, button2),  windows . (W.shiftMaster .) . W.focusWindow)
  , ((alt, button3),  \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  , ((modk, button3), \w -> focus w >> withFocused (windows . W.sink))
  ]

kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
  override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
  wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
  return $ maybe False (elem $ fromIntegral override) wt

main :: IO ()
main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  setCurrentDirectory "/home/sandy"
  let space = 5
      border = Border space space space space

  xmonad $ ewmh $ docks def
    { borderWidth        = 1
    , terminal           = "xfce4-terminal"
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#444444"
    , workspaces = myWorkspaces
    , modMask = modk
    , logHook = dynamicLogWithPP $ myLogHook dbus
    , startupHook = setWMName "LG3D"
    , layoutHook  = spacingRaw True border True border True $ smartBorders myLayout
    , manageHook  = mconcat [ manageDocks
                            , myManageHook
                            , manageHook def
                            ]
    , handleEventHook = dynamicPropertyChange "WM_CLASS" myDynamicManageHook <> fullscreenEventHook
    } `removeKeys`              keysToUnbind
      `additionalKeys`          keysToBind
      `removeMouseBindings`     buttonsToUnbind
      `additionalMouseBindings` buttonsToBind


-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
myLogHook :: D.Client -> PP
myLogHook dbus = def
  { ppOutput = dbusOutput dbus
  , ppTitle   = wrap ("%{+u} ") " %{-u}"
  , ppCurrent = wrap ("%{+o} ") " %{-o}"
  , ppVisible = wrap ("%{+u} ") " %{-u}"
  , ppSep     = "   "
  , ppLayout  = const ""
  }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

