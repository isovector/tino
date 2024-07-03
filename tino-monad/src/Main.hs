{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Main where

import qualified Codec.Binary.UTF8.String as UTF8
import           Control.Exception
import           Control.Monad
import           Data.Char (isSpace)
import           Data.Foldable
import           Data.IORef
import           Data.List (sort, intercalate, isInfixOf)
import           Data.Maybe (fromJust)
import           Data.Monoid (Endo (..), All(..))
import           Data.Ratio
import qualified Data.Text as T
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
import           System.Process (readProcessWithExitCode, shell, readCreateProcess)
import           Text.Show.Unicode
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
import           XMonad.Layout.Drawer
import           XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutBuilder
import           XMonad.Layout.MagicFocus
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Prompt (greenXPConfig, XPConfig(font))
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys, removeKeys, additionalMouseBindings, removeMouseBindings)
import           XMonad.Util.Hacks
import           XMonad.Util.Run (safeSpawnProg, safeSpawn, spawnPipe, hPutStrLn, runProcessWithInput, runInTerm)
import           XMonad.Util.Ungrab
import           XMonad.Util.WindowProperties (getProp32s)
import           XPcfb


myWorkspaces :: [String]
myWorkspaces = fmap show [1..10]


getDirectories :: MonadIO m => FilePath -> m [String]
getDirectories fp = liftIO $ do
  files <- fmap (fp </>) <$> listDirectory fp
  let is_dir = fmap (fileTypeIsDirectory . fileTypeFromMetadata) . getFileMetadata
  filterM is_dir $ sort files


-- NOTE TO SELF: doing anything with processes is super liable to explode and
-- not work for the dumbest reasons. Make sure to ALWAYS call 'readProcess'
-- rather than using anything from the @process@ lib directly.
readProcess :: String -> [String] -> String -> X String
readProcess prog args input = do
    unGrab
    runProcessWithInput prog args input


-- NOTE TO SELF: doing anything with processes is super liable to explode and
-- not work for the dumbest reasons. Make sure to ALWAYS call 'readProcess'
-- rather than using anything from the @process@ lib directly.
rofi :: String -> [String] -> X (Maybe String)
rofi prompt actions = do
    unGrab
    out <- lines <$> runProcessWithInput "rofi" ["-dmenu", "-i", "-location", "0", "-p", prompt] (unlines actions)
    pure $ case out of
        [sel] -> Just sel
        _ -> Nothing


myManageHook :: Query (Endo WindowSet)
myManageHook = fold
  [ resource  =? "desktop_window"  --> doIgnore
  , className =? "stalonetray"     --> doIgnore
  , className =? "blueman-applet"  --> doIgnore
  , className =? "anki"            --> doFloat
  , className =? "Anki"            --> doFloat
  -- , className =? "vlc"             --> doFloat
  , className =? "Spotify"         --> doShift "music"
  , className =? "Signal"          --> do
        doSink
        doShift "comm"
  , title     =? "New entry"       --> doFloat
  , role      =? "conversation"    --> doSideFloat SE
  , kdeOverride                    --> doFloat
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


myterm = "kitty"

mydrawer = simpleDrawer 0.01 0.3 (ClassName myterm)

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
polybar = safeSpawn' "eww" "reload"


keysToBind :: IORef Bool -> [((KeyMask, KeySym), X ())]
keysToBind ref =
  [ ((modk, xK_f),                  -- safeSpawn "/usr/bin/kitty" ["/usr/bin/w3m",  "/home/sandy/.rawdog/output.html"])
                                    runOrRaise "brave" [] $ className =? "brave")
  , ((modk, xK_g),                  runOrRaise "neovide" ["--no-multigrid"] $ className =? "neovide")
  -- , ((modk .|. alt, xK_g),          runInTerm "" "neomutt")
  , ((modk .|. alt, xK_g),          safeSpawn' "evolution" "-c mail")
  , ((modk .|. alt, xK_c),          safeSpawn' "evolution" "-c calendar")
  , ((modk, xK_s),                  runOrRaise "signal-desktop" [] $ className =? "Signal")
  , ((modk, xK_d),                  safeSpawn' "rofi" "-show run")
  -- , ((modk, xK_s),                  safeSpawn' "/home/sandy/.tino/bin/rofi-find" "")
  , ((modk, xK_h),                  spawn "/home/sandy/.tino/bin/rofi-hackage")
  , ((modk, xK_e),                  manigaJJEdit)
  , ((modk, xK_n),                  manigaJJNew)
  , ((modk .|. shiftMask, xK_n),    manigaJJDesc)
  , ((modk, xK_r),                  safeSpawn' "eww" "reload")
  , ((modk, xK_backslash),          polybar)
  -- , ((modk, xK_b),                  safeSpawn' "/home/sandy/.tino/bin/rofi-web" "")
  , ((modk, xK_b),                  bluetooth)
  , ((modk, xK_m),                  manigaPullRequests)
  , ((modk .|. shiftMask, xK_b),    safeSpawn "/home/sandy/.tino/bin/connect-bt" [])
  , ((modk, xK_x),                  safeSpawnProg myterm)
  , ((modk, xK_t),                  safeSpawnProg "thunar")
  , ((modk .|. shiftMask, xK_q),    kill)
  , ((modk, xK_p),                  safeSpawnProg "scrot")
  , ((modk .|. shiftMask, xK_p),    spawn "sleep 0.2; scrot -s")
  , ((ctrlk, xK_F3),               safeSpawn' "amixer" "-c 1 -q set Master 2dB+")
  , ((ctrlk, xK_F2),               safeSpawn' "amixer" "-c 1 -q set Master 2dB-")
  , ((modk, xK_F8),               safeSpawn' "/home/sandy/.tino/bin/backlight" "-5")
  , ((modk .|. shiftMask, xK_F8), safeSpawn' "/home/sandy/.tino/bin/backlight" "-15")
  , ((modk, xK_F9),               safeSpawn' "/home/sandy/.tino/bin/backlight" "5")
  , ((modk .|. shiftMask, xK_F9), safeSpawn' "/home/sandy/.tino/bin/backlight" "15")
  , ((0, xF86XK_MonBrightnessDown),               safeSpawn' "/home/sandy/.tino/bin/backlight" "-5")
  , ((shiftMask, xF86XK_MonBrightnessDown), safeSpawn' "/home/sandy/.tino/bin/backlight" "-15")
  , ((0, xF86XK_MonBrightnessUp),               safeSpawn' "/home/sandy/.tino/bin/backlight" "5")
  , ((shiftMask, xF86XK_MonBrightnessUp), safeSpawn' "/home/sandy/.tino/bin/backlight" "15")
  , ((modk .|. shiftMask, xK_h),    sendMessage Shrink)
  , ((modk .|. shiftMask, xK_l),    sendMessage Expand)
  , ((modk, xK_F7), do
      safeSpawn' "/home/sandy/.tino/bin/external-monitor" ""
      feh
    )
  , ((modk .|. shiftMask, xK_F7), do
      safeSpawn' "xrandr" "--output DP-1 --off --output DP-2 --off --output HDMI-1 --off"
      polybar
    )
  , ((modk .|. shiftMask, xK_F12),  safeSpawn' "redshift" "-x")
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

  , ((modk, xK_comma), spawn "eww update revealInfo=true; sleep 3s; eww update revealInfo=false")
  -- , ((modk .|. alt .|. ctrlk, xK_k), hass "homeassistant.toggle" "entity_id=switch.kitchen_light")
  -- , ((modk .|. alt .|. ctrlk, xK_l), hass "homeassistant.toggle" "entity_id=light.living_room_lights")
  ] ++ fmap (uncurry mkShortcut) shortcuts

hass :: String -> String -> X ()
hass service args =
  safeSpawn'
    "hass-cli" $
    "--token eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiI0NGY5MDVhZGU3Mjg0ZDY3YWMyYjlhNWJjMGQ0MzJkZSIsImlhdCI6MTY3NzExMTY5NCwiZXhwIjoxOTkyNDcxNjk0fQ.I3Ble7B9lRIONfPmHb42TKr0OjcITzzczZ7Elk0B4Pw -s http://192.168.1.2:8123 service call " <> service <> " --arguments " <> args



-- NOTE TO SELF: doing anything with processes is super liable to explode and
-- not work for the dumbest reasons. Make sure to ALWAYS call 'readProcess'
-- rather than using anything from the @process@ lib directly.
bluetooth :: X ()
bluetooth = do
  bts <- fmap ( filter (not . isInfixOf "Pro Controller")
              . lines
              . T.unpack
              . T.replace "PYLEUSA" "Kitchen"
              . T.replace "BE-RCA" "Living Room"
              . T.pack
              )
       $ readProcess "bluetoothctl" ["devices", "Paired"] ""
  x <- rofi "Devices" bts
  for_ x $ \bt -> do
    let dev = words bt !! 1
    safeSpawn "/home/sandy/.tino/bin/connect-bt" [dev]


-- NOTE TO SELF: doing anything with processes is super liable to explode and
-- not work for the dumbest reasons. Make sure to ALWAYS call 'readProcess'
-- rather than using anything from the @process@ lib directly.
haskellProject :: X ()
haskellProject = do
  dirs <- getDirectories "/home/sandy/prj"
  x <- rofi "Project" dirs
  case x of
    Just prj -> do
      liftIO $ withCurrentDirectory prj $ do
        let target = case prj of
                       "/home/sandy/prj/maniga" -> "maniga:lib"
                       _ -> ""
        safeSpawn "neovide" ["--no-multigrid"]
        safeSpawn myterm ["--command", "tmux new-session 'stack repl " <> target <> "'"]
    Nothing -> pure ()


-- NOTE TO SELF: doing anything with processes is super liable to explode and
-- not work for the dumbest reasons. Make sure to ALWAYS call 'readProcess'
-- rather than using anything from the @process@ lib directly.
manigaPullRequests :: X ()
manigaPullRequests = do
  liftIO $ setCurrentDirectory "/home/sandy/prj/manifold/maniga"
  rawPrs <- readProcess "gh" ["pr", "list"] ""
  liftIO $ setCurrentDirectory homeDir
  let prs = sort
          $ fmap (T.unpack . T.intercalate "\t" . take 2 . filter (not . T.null) . T.splitOn "\t" .  T.pack)
          $ lines rawPrs
  x <- rofi "Maniga PRs" prs
  for_ x $ \pr ->
    safeSpawn' "xdg-open" $
      "https://github.com/manifoldvalley/maniga/pull/" <> takeWhile (not . isSpace) pr

jjProject = "/home/sandy/prj/manifold/maniga"

manigaJJEdit :: X ()
manigaJJEdit = do
  liftIO $ setCurrentDirectory jjProject
  safeSpawn' "/home/sandy/.tino/bin/e" ""
  liftIO $ setCurrentDirectory homeDir

manigaJJNew :: X ()
manigaJJNew = do
  liftIO $ setCurrentDirectory jjProject
  safeSpawn' "/home/sandy/.tino/bin/jj-rofi" "new"
  liftIO $ setCurrentDirectory homeDir

manigaJJDesc :: X ()
manigaJJDesc = do
  liftIO $ setCurrentDirectory jjProject
  safeSpawn' "/home/sandy/.tino/bin/jj-rofi" "describe"
  liftIO $ setCurrentDirectory homeDir


myPP :: PP
myPP = def
  { ppCurrent = \name -> "\"" <> name <> "\": \"current active\""
  , ppHidden  = \name -> "\"" <> name <> "\": \"active\""
  , ppVisible = \name -> "\"" <> name <> "\": \"active visible\""
  , ppWsSep = ", "
  , ppTitle = mappend "\"title\": "
            . ushow
            . T.unpack
            . T.replace "- NVIM" ""
            . T.replace "- Brave" ""
            . T.replace "- YouTube" ""
            . T.pack
  , ppLayout = \l -> "\"layout\": " <> show l
  , ppOrder = pure . wrap "{" "}" . intercalate ", "
  , ppSep = ", "
  }

myStatusBar :: StatusBarConfig
myStatusBar = mconcat
  [ statusBarProp "eww open bar0" $ pure myPP
  , statusBarProp "eww open bar1" $ pure myPP
  , statusBarProp "eww open bar2" $ pure myPP
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
  , (xK_w, "https://workflowy.com")
  -- , (xK_b, "https://docs.google.com/forms/d/e/1FAIpQLSdHnF9PrE2FQNopHcdJnz0xEXpAKIFb_lShzBzbCpPphyzFdA/viewform")
  -- , (xK_j, "https://next.waveapps.com/5ff1dd74-11d9-4710-83a3-534a35ce9e70/invoices/1922138192530766354/edit")
  , (xK_p, "https://clients.mindbodyonline.com/classic/ws?studioid=30617")
  , (xK_t, "https://www.rememberthemilk.com/app/#list/48436173")
  , (xK_a, "http://192.168.1.2:8123/")
  , (xK_y, "http://100.92.132.112:8451")
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

homeDir :: FilePath
homeDir = "/home/sandy"

main :: IO ()
main = do
  mouseToggleIORef <- newIORef True

  setCurrentDirectory homeDir
  let space = 5
      border = Border space space space space

  xmonad $ withSB myStatusBar $ docks $ ewmh $
    def
    { borderWidth        = 1
    , terminal           = myterm
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#444444"
    , workspaces = myWorkspaces
    , modMask = modk
    , startupHook = mconcat
        [ setWMName "LG3D"
        , feh
        , spawn "xsetroot -cursor_name left_ptr"
        , spawn "eww daemon"
        , docksStartupHook
        ]
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

