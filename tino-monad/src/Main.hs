{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE NumDecimals #-}

module Main where

import qualified Codec.Binary.UTF8.String as UTF8
import           Control.Exception
import           Control.Monad
import           Data.Foldable
import           Data.IORef
import           Data.List (sort)
import           Data.Maybe (fromJust)
import           Data.Monoid (Endo (..), All(..))
import           Data.Ratio
import           Data.Word (Word32)
import           GHC.Exts (fromString)
import           Graphics.X11.ExtraTypes.XF86
import           Lights
import           System.Directory (setCurrentDirectory, withCurrentDirectory, listDirectory)
import           System.Directory.Internal (fileTypeIsDirectory, getFileMetadata, fileTypeFromMetadata)
import           System.Exit
import           System.FilePath
import           System.IO (hGetContents, Handle)
import           System.IO.Capture (capture)
import           System.Process (readProcessWithExitCode, readProcess)
import           XMonad hiding (getDirectories)
import           XMonad.Actions.CopyWindow (copyToAll)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Search hiding (Query)
import           XMonad.Actions.WindowGo (raiseMaybe)
import           XMonad.Actions.WorkspaceNames
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import           XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, docksStartupHook, docksEventHook)
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen, doSideFloat, Side (SE, NE), doRectFloat)
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import           XMonad.Layout.LayoutBuilder
import           XMonad.Layout.MagicFocus
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.Grid
import           XMonad.Layout.Drawer
import           XMonad.Prompt (greenXPConfig, XPConfig(font))
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys, removeKeys, additionalMouseBindings, removeMouseBindings)
import           XMonad.Util.Hacks
import           XMonad.Util.Run (safeSpawnProg, safeSpawn, spawnPipe, hPutStrLn, runProcessWithInput, runInTerm)
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
  , "command"
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
       then doFloat >> doF copyToAll
       else mempty
  , isFullscreen                  --> doFullFloat
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

myLayout = -- onLeft mydrawer $
    avoidStruts
    ( Tall 1 (3/100) (1/2)
  ||| ThreeColMid 1 (3/100) (1/2)
  -- ||| simpleTabbed
  -- ||| Mirror (Tall 1 (3/100) (1/2))
  ||| Full
  -- ||| spiral (6/7)
  ||| emptyBSP
  -- ||| Grid
    )
  ||| noBorders (fullscreenFull Full)


mydrawer = simpleDrawer 0.01 0.3 (ClassName "Xfce4-terminal")

runOrRaise :: String -> [String] -> Query Bool -> X ()
runOrRaise = (raiseMaybe .) . safeSpawn

alt :: KeyMask
alt  = mod1Mask

musk :: KeyMask
musk = mod3Mask

modk :: KeyMask
modk = mod4Mask

ctrlk :: KeyMask
ctrlk = controlMask

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


keysToBind :: IORef Bool -> [((KeyMask, KeySym), X ())]
keysToBind ref =
  [ ((modk, xK_f),                  runOrRaise "brave" [] $ className =? "brave")
  , ((modk, xK_g),                  runOrRaise "neovide" [] $ className =? "neovide")
  , ((modk, xK_m),                  runOrRaise "spotify" [] $ className =? "Spotify")
  -- , ((modk .|. alt, xK_g),          runInTerm "" "neomutt")
  , ((modk .|. alt, xK_g),          runOrRaise "evolution" [] $ className =? "Evolution")
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
  , ((ctrlk, xK_F3),               safeSpawn' "amixer" "-c 1 -q set Master 2dB+")
  , ((ctrlk, xK_F2),               safeSpawn' "amixer" "-c 1 -q set Master 2dB-")
  , ((ctrlk, xK_F5),               safeSpawn' "/home/sandy/.tino/bin/backlight" "-1")
  , ((ctrlk .|. shiftMask, xK_F5), safeSpawn' "/home/sandy/.tino/bin/backlight" "-5")
  , ((ctrlk, xK_F6),               safeSpawn' "/home/sandy/.tino/bin/backlight" "1")
  , ((ctrlk .|. shiftMask, xK_F6), safeSpawn' "/home/sandy/.tino/bin/backlight" "5")
  , ((modk .|. shiftMask, xK_h),    sendMessage Shrink)
  , ((modk .|. shiftMask, xK_l),    sendMessage Expand)
  , ((modk, xK_F10), do
      safeSpawn' "/home/sandy/.tino/bin/external-monitor" ""
      feh
      polybar
    )
  , ((modk, xK_F9), do
      safeSpawn' "xrandr" "--output DP-2 --off --output HDMI-1 --off"
      polybar
    )
  , ((modk, xK_F8), do
      safeSpawn' "xrandr" "--output HDMI-1 --brightness 0.5"
      safeSpawn' "xrandr" "--output DP-2 --brightness 0.5"
      safeSpawn' "xrandr" "--output eDP-1 --brightness 0.5"
    )
  , ((modk, xK_F11),                safeSpawn' "redshift" "-x")
  , ((modk, xK_F12),                safeSpawn' "redshift" "-O1500")
  , ((modk, xK_c),                  rofi "Start Project" [] >>= pcfbPrompt . fromJust)
  , ((modk, xK_0),                  windows $ W.greedyView "command")
  -- , ((modk, xK_Left),               liftIO $ setDeskColor 40 0 40)
  -- , ((modk, xK_Right),              liftIO $ setDeskColor 60 20 0)
  -- , ((modk, xK_Down),               liftIO $ setDeskColor 0 0 0)
  -- , ((modk, xK_Up),                 liftIO $ setDeskColor 0 0 80)
  , ((modk, xK_v),                  safeSpawn' "/home/sandy/.tino/bin/tino" "pcfb")
  , ((modk, xK_bracketleft),        liftIO pcfbOpen)
  , ((modk, xK_bracketright),       liftIO pcfbClose)
  , ((modk .|. ctrlk, xK_l),  do
        sid <- withWindowSet $ pure . drop 2 . show . W.screen . W.current
        spawn $ "eww close powermenu || eww open powermenu --screen " <> sid)
  , ((modk .|. ctrlk, xK_h),  safeSpawn' "systemctl" "suspend")
  , ((modk .|. ctrlk, xK_f),  withFocused $ windows . W.sink)
  -- , ((modk .|. ctrlk, xK_m),  liftIO $ modifyIORef' ref not)
  , ((musk, xK_Left),                safeSpawn' "/home/sandy/.tino/bin/playerctl-fast" "previous")
  , ((0, xF86XK_AudioPrev),          safeSpawn' "/home/sandy/.tino/bin/playerctl-fast" "previous")
  , ((musk, xK_Right),               safeSpawn' "/home/sandy/.tino/bin/playerctl-fast" "next")
  , ((0, xF86XK_AudioNext),          safeSpawn' "/home/sandy/.tino/bin/playerctl-fast" "next")
  , ((musk, xK_Down),                safeSpawn' "/home/sandy/.tino/bin/playerctl-fast" "play-pause")
  , ((0, xF86XK_AudioPlay),          safeSpawn' "/home/sandy/.tino/bin/playerctl-fast" "play-pause")
  -- seems silly but my screens are backwards from their physical setup
  , ((modk, xK_Left),                nextScreen)
  , ((modk, xK_Right),               prevScreen)
  , ((modk .|. shiftMask, xK_Left),  shiftNextScreen >> nextScreen)
  , ((modk .|. shiftMask, xK_Right), shiftPrevScreen >> prevScreen)
  , ((modk .|. alt .|. ctrlk, xK_k), hass "homeassistant.toggle" "entity_id=switch.kitchen_light")
  , ((modk .|. alt .|. ctrlk, xK_l), hass "homeassistant.toggle" "entity_id=light.living_room_lights")
  ] ++ fmap (uncurry mkShortcut) shortcuts

hass :: String -> String -> X ()
hass service args =
  safeSpawn'
    "hass-cli" $
    "--token eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiI0NGY5MDVhZGU3Mjg0ZDY3YWMyYjlhNWJjMGQ0MzJkZSIsImlhdCI6MTY3NzExMTY5NCwiZXhwIjoxOTkyNDcxNjk0fQ.I3Ble7B9lRIONfPmHb42TKr0OjcITzzczZ7Elk0B4Pw -s http://192.168.1.2:8123 service call " <> service <> " --arguments " <> args


haskellProject :: X ()
haskellProject = do
  dirs <- getDirectories "/home/sandy/prj"
  x <- rofi "Project" dirs
  case x of
    Just prj -> do
      liftIO $ withCurrentDirectory prj $ do
        let target = case prj of
                       "/home/sandy/prj/hls" -> "hls-tactics-plugin:lib"
                       "/home/sandy/prj/marlo" -> "marlo:lib"
                       "/home/sandy/prj/wire-server" -> "brig:lib"
                       "/home/sandy/prj/maniga" -> "maniga:lib"
                       _ -> ""
        safeSpawn "neovide" []
        safeSpawn "xfce4-terminal" ["--command", "tmux new-session 'stack repl " <> target <> "'"]
    Nothing -> pure ()

myPP :: PP
myPP = def
  { ppCurrent = xmobarColor "#ff7700" "" . wrap "[" "]"
  , ppTitle   = id
  , ppVisible = wrap "(" ")"
  , ppLayout  = id
  , ppUrgent  = xmobarColor "red" "yellow"
  }

myStatusBar :: StatusBarConfig
myStatusBar = mconcat
  [ statusBarProp "xmobar -x 0" (pure myPP)
  , statusBarProp "xmobar -x 1" (pure myPP)
  , statusBarProp "xmobar -x 2" (pure myPP)
  ]


mkShortcut :: MonadIO m => KeySym -> String -> ((KeyMask, KeySym), m ())
mkShortcut ks url =
  ((modk .|. alt, ks), safeSpawn' "xdg-open" url)

shortcuts :: [(KeySym, String)]
shortcuts =
  [
    (xK_f, "https://riot.cofree.coffee")
  , (xK_d, "file:///home/sandy/.rawdog/output.html")
  , (xK_m, "https://maps.google.com")
  , (xK_h, "https://github.com/pulls")
  , (xK_c, "https://mx.sandymaguire.me/SOGo/so/sandy@sandymaguire.me/Calendar/view")
  , (xK_w, "https://workflowy.com")
  , (xK_b, "https://docs.google.com/forms/d/e/1FAIpQLSdHnF9PrE2FQNopHcdJnz0xEXpAKIFb_lShzBzbCpPphyzFdA/viewform")
  , (xK_j, "https://next.waveapps.com/5ff1dd74-11d9-4710-83a3-534a35ce9e70/invoices/1808331315003652408/edit")
  , (xK_p, "https://clients.mindbodyonline.com/classic/ws?studioid=30617")
  , (xK_t, "https://www.rememberthemilk.com/app/#all")
  , (xK_a, "http://192.168.1.2:8123/")
  ]

buttonsToUnbind :: [(KeyMask, Button)]
buttonsToUnbind =
  [ (modk, button1)
  , (modk, button2)
  , (modk, button3)
  ]

onlyWhenIORef :: IORef Bool -> X () -> X ()
onlyWhenIORef ref m = do
  liftIO (readIORef ref) >>= \case
    True -> m
    False -> pure ()


buttonsToBind :: IORef Bool -> [((KeyMask, Button), Window -> X ())]
buttonsToBind ioref =
  [ ((alt, button1),  \w -> onlyWhenIORef ioref $ focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , ((alt, button2),  \w -> onlyWhenIORef ioref $ windows . (W.shiftMaster .) $ W.focusWindow w)
  , ((alt, button3),  \w -> onlyWhenIORef ioref $ focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  , ((modk, button3), \w -> onlyWhenIORef ioref $ focus w >> withFocused (windows . W.sink))
  ]

kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
  override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
  wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
  return $ maybe False (elem $ fromIntegral override) wt

feh :: X ()
feh =
  spawn "feh --bg-fill ./.wallpapers/eDP-1.jpg ./.wallpapers/HDMI-1.jpg ./.wallpapers/DP-1.jpg"

main :: IO ()
main = do
  mouseToggleIORef <- newIORef True

  setCurrentDirectory "/home/sandy"
  let space = 5
      border = Border space space space space

  xmonad $ withSB myStatusBar $ docks $ ewmh $
    def
    { borderWidth        = 1
    , terminal           = "xfce4-terminal"
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#444444"
    , workspaces = myWorkspaces
    , modMask = modk
    , startupHook = setWMName "LG3D" <> feh <> docksStartupHook
    , layoutHook  = avoidStruts $ smartBorders myLayout
    , focusFollowsMouse = True
    , manageHook  = mconcat [ manageDocks
                            , myManageHook
                            ]
    , handleEventHook = mconcat
        [ dynamicPropertyChange "WM_CLASS" myDynamicManageHook
        , dynamicPropertyChange "WM_NAME" myDynamicManageHook
        , docksEventHook
        , windowedFullscreenFixEventHook
        -- , followOnlyIf shouldFollow
        ]
    } `removeKeys`              keysToUnbind
      `additionalKeys`          (keysToBind mouseToggleIORef)
      `removeMouseBindings`     buttonsToUnbind
      `additionalMouseBindings` (buttonsToBind mouseToggleIORef)


setTransparentHook :: Event -> X All
setTransparentHook ConfigureEvent{ev_event_type = createNotify, ev_window = id} = do
  setOpacity id opacity
  return (All True) where
    opacityFloat = 0.9
    opacity = floor $ fromIntegral (maxBound :: Word32) * opacityFloat
    setOpacity id op = spawn $ "xprop -id " ++ show id ++ " -f _NET_WM_WINDOW_OPACITY 32c -set _NET_WM_WINDOW_OPACITY " ++ show op
setTransparentHook _ = return (All True)

