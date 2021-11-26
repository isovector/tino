{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE NumDecimals #-}

module Main where

import Lights
import qualified Codec.Binary.UTF8.String as UTF8
import           Control.Exception
import           Control.Monad
import qualified DBus as D
import qualified DBus.Client as D
import           Data.Foldable
import           Data.List (sort)
import           Data.Maybe (fromJust)
import           Data.Monoid (Endo (..), All(..))
import           Data.Ratio
import           Data.Word (Word32)
import           GHC.Exts (fromString)
import           Graphics.X11.ExtraTypes.XF86
import           System.Directory (setCurrentDirectory, withCurrentDirectory, listDirectory)
import           System.Directory.Internal (fileTypeIsDirectory, getFileMetadata, fileTypeFromMetadata)
import           System.Exit
import           System.FilePath
import           System.IO (hGetContents)
import           System.IO.Capture (capture)
import           System.Process (readProcessWithExitCode, readProcess)
import           XMonad
import           XMonad.Actions.CopyWindow (copyToAll)
import           XMonad.Actions.Search hiding (Query)
import           XMonad.Actions.WindowGo (raiseMaybe)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import           XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, docksStartupHook, docksEventHook)
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen, doSideFloat, Side (SE, NE), doRectFloat)
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Layout.Accordion
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.ThreeColumns
import           XMonad.Prompt (greenXPConfig, XPConfig(font))
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys, removeKeys, additionalMouseBindings, removeMouseBindings)
import           XMonad.Util.Run (safeSpawnProg, safeSpawn, spawnPipe, hPutStrLn, runProcessWithInput)
import           XMonad.Util.Ungrab
import           XMonad.Util.WindowProperties (getProp32s)
import           XPcfb


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


getDirectories :: MonadIO m => FilePath -> m [String]
getDirectories fp = liftIO $ do
  files <- fmap (fp </>) <$> listDirectory fp
  let is_dir = fmap (fileTypeIsDirectory . fileTypeFromMetadata) . getFileMetadata
  filterM is_dir $ sort files


rofi :: String -> [String] -> X (Maybe String)
rofi prompt actions = do
    unGrab
    out <- lines <$> runProcessWithInput "rofi" ["-dmenu", "-i", "-location", "0", "-p", prompt] (unlines actions)
    pure $ case out of
        [sel] -> Just sel
        _ -> Nothing


myManageHook :: Query (Endo WindowSet)
myManageHook = fold
    [ resource  =? "desktop_window" --> doIgnore
    , className =? "stalonetray"    --> doIgnore
    , className =? "anki"           --> doFloat
    , className =? "Anki"           --> doFloat
    , className =? "vlc"            --> doFloat
    , className =? "Spotify"        --> doShift "music"
    , className =? "Signal"         --> do
        doSink
        doShift "comm"
    , title     =? "New entry"      --> doFloat
    , role      =? "conversation"   --> doSideFloat SE
    , kdeOverride                   --> doFloat
    , do
        c <- className
        if c == "zoom"
           then doF copyToAll
           else mempty
    -- , isFullscreen                  --> doFullFloat
    ]


doSink :: ManageHook
doSink = ask >>= doF . W.sink

myDynamicManageHook :: Query (Endo WindowSet)
myDynamicManageHook = fold
    [ className =? "Spotify" --> doShift "music"
    , className =? "Signal"  --> doShift "comm"
    , name     =? "Percentile Feedback" --> doRectFloat (W.RationalRect (2%3) 0 (1%3) (3%10))
    ]


role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

name :: Query String
name = stringProperty "WM_NAME"


myLayout =
    avoidStruts
    ( Tall 1 (3/100) (1/2)
  -- ||| ThreeColMid 1 (3/100) (1/2)
  ||| Accordion
  -- ||| Mirror (Tall 1 (3/100) (1/2))
  ||| Full
  -- ||| spiral (6/7)
  -- ||| emptyBSP
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

polybar :: X ()
polybar = pure () -- safeSpawn' "/home/sandy/.tino/bin/tino" "bar"

keysToBind :: [((KeyMask, KeySym), X ())]
keysToBind =
  [ ((modk, xK_f),                  runOrRaise "brave" [] $ className =? "brave")
  , ((modk, xK_g),                  runOrRaise "neovide" [] $ className =? "neovide")
  , ((modk, xK_m),                  runOrRaise "spotify" [] $ className =? "Spotify")
  , ((modk, xK_s),                  runOrRaise "signal-desktop" [] $ className =? "Signal")
  , ((modk, xK_d),                  safeSpawn' "rofi" "-show run")
  -- , ((modk, xK_s),                  safeSpawn' "/home/sandy/.tino/bin/rofi-find" "")
  , ((modk, xK_h),                  spawn "/home/sandy/.tino/bin/rofi-hackage")
  , ((modk, xK_e),                  haskellProject)
  , ((modk, xK_backslash),          polybar)
  , ((modk, xK_b),                  safeSpawn' "/home/sandy/.tino/bin/rofi-web" "")
  , ((modk, xK_x),                  safeSpawnProg "xfce4-terminal")
  , ((modk, xK_t),                  safeSpawnProg "thunar")
  , ((modk .|. shiftMask, xK_q),    kill)
  , ((modk, xK_p),                  safeSpawnProg "scrot")
  , ((modk .|. shiftMask, xK_p),    spawn "sleep 0.2; scrot -s")
  , ((0, xF86XK_AudioRaiseVolume),  safeSpawn' "amixer" "-c 1 -q set Master 2dB+")
  , ((0, xF86XK_AudioLowerVolume),  safeSpawn' "amixer" "-c 1 -q set Master 2dB-")
  , ((0, xF86XK_MonBrightnessDown), safeSpawn' "/home/sandy/.tino/bin/backlight" "-10")
  , ((0, xF86XK_MonBrightnessUp),   safeSpawn' "/home/sandy/.tino/bin/backlight" "5")
  , ((modk .|. shiftMask, xK_h),    sendMessage Shrink)
  , ((modk .|. shiftMask, xK_l),    sendMessage Expand)
  , ((modk, xK_F10), do
      safeSpawn' "xrandr" "--output HDMI-1 --mode 1920x1080 --left-of eDP-1 --output DP-2 --mode 1920x1080 --left-of HDMI-1 --rotate left"
      polybar
    )
  , ((modk, xK_F9), do
      safeSpawn' "xrandr" "--output DP-2 --off --output HDMI-1 --off"
      polybar
    )
  , ((modk, xK_F11),                safeSpawn' "redshift" "-x")
  , ((modk, xK_F12),                safeSpawn' "redshift" "-O1500")
  , ((modk, xK_c),                  rofi "Start Project" [] >>= pcfbPrompt . fromJust)
  , ((modk, xK_Left),               liftIO $ setDeskColor 40 0 40)
  , ((modk, xK_Right),              liftIO $ setDeskColor 60 20 0)
  , ((modk, xK_Down),               liftIO $ setDeskColor 0 0 0)
  , ((modk, xK_Up),                 liftIO $ setDeskColor 0 0 80)
  , ((modk, xK_v),                  safeSpawn' "/home/sandy/.tino/bin/tino" "pcfb")
  , ((modk, xK_bracketleft),        liftIO pcfbOpen)
  , ((modk, xK_bracketright),       liftIO pcfbClose)
  , ((modk .|. controlMask, xK_l),  safeSpawn' "dm-tool" "lock")
  , ((modk .|. controlMask, xK_h),  safeSpawn' "systemctl" "suspend")
  , ((modk .|. controlMask, xK_f),  withFocused $ windows . W.sink)
  , ((musk, xK_Left),               safeSpawn' "playerctl" "previous --player=spotify")
  , ((0, xF86XK_AudioPrev),         safeSpawn' "playerctl" "previous --player=spotify")
  , ((musk, xK_Right),              safeSpawn' "playerctl" "next --player=spotify")
  , ((0, xF86XK_AudioNext),         safeSpawn' "playerctl" "next --player=spotify")
  , ((musk, xK_Down),               safeSpawn' "playerctl" "play-pause --player=spotify")
  , ((0, xF86XK_AudioPlay),         safeSpawn' "playerctl" "play-pause --player=spotify")
  ] ++ fmap (uncurry mkShortcut) shortcuts


haskellProject :: X ()
haskellProject = do
  dirs <- getDirectories "/home/sandy/prj"
  x <- rofi "Project" dirs
  case x of
    Just prj -> do
      liftIO $ withCurrentDirectory prj $ do
        let target = case prj of
                       "/home/sandy/prj/hls" -> "hls-tactics-plugin:lib"
                       "/home/sandy/prj/wire-server" -> "spar:lib"
                       _ -> ""
        safeSpawn "neovide" []
        safeSpawn "xfce4-terminal" ["--command", "tmux new-session 'stack repl " <> target <> "'"]
    Nothing -> pure ()




mkShortcut :: MonadIO m => KeySym -> String -> ((KeyMask, KeySym), m ())
mkShortcut ks url =
  ((modk .|. alt, ks), safeSpawn' "xdg-open" url)

shortcuts :: [(KeySym, String)]
shortcuts =
  [ (xK_g, "https://gmail.com")
  , (xK_r, "https://reddit.com")
  , (xK_f, "https://riot.cofree.coffee")
  , (xK_d, "file:///home/sandy/.rawdog/output.html")
  , (xK_m, "https://maps.google.com")
  , (xK_2, "https://docs.google.com/spreadsheets/d/1g-uY0BjO0yNiID6obpDuj8uEeaqW3MExF_PfhqVYRXg/edit#gid=0")
  , (xK_h, "https://github.com/pulls")
  , (xK_t, "https://trello.com/b/y2C9T3x2/copilot")
  , (xK_i, "https://github.com/haskell/haskell-language-server/issues/new")
  , (xK_c, "https://calendar.google.com/calendar/u/0/r")
  , (xK_w, "https://workflowy.com")
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

  xmonad $ docks $ ewmh $
    def
    { borderWidth        = 1
    , terminal           = "xfce4-terminal"
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#444444"
    , workspaces = myWorkspaces
    , modMask = modk
    , logHook = dynamicLogWithPP $ myLogHook dbus
    , startupHook = setWMName "LG3D"
    , layoutHook  = avoidStruts $ smartBorders myLayout
    , manageHook  = mconcat [ manageDocks
                            , myManageHook
                            ]
    , handleEventHook = mconcat
        [ docksEventHook
        , dynamicPropertyChange "WM_CLASS" myDynamicManageHook
        , dynamicPropertyChange "WM_NAME" myDynamicManageHook
        , fullscreenEventHook
        ]
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

setTransparentHook :: Event -> X All
setTransparentHook ConfigureEvent{ev_event_type = createNotify, ev_window = id} = do
  setOpacity id opacity
  return (All True) where
    opacityFloat = 0.9
    opacity = floor $ fromIntegral (maxBound :: Word32) * opacityFloat
    setOpacity id op = spawn $ "xprop -id " ++ show id ++ " -f _NET_WM_WINDOW_OPACITY 32c -set _NET_WM_WINDOW_OPACITY " ++ show op
setTransparentHook _ = return (All True)

