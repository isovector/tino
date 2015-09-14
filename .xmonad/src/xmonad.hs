import Prelude hiding (mod)

import Control.Monad (when, liftM2)
import Data.List (isInfixOf)
import Data.Monoid (mconcat)
import Graphics.X11.ExtraTypes.XF86
import System.Environment (getArgs)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import XPcfb

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.EZConfig ( additionalKeys
                            , removeKeys
                            , additionalMouseBindings
                            , removeMouseBindings
                            )
import XMonad.Util.Replace (replace)
import XMonad.Util.Run
import qualified XMonad.StackSet as W

data Machine = HomeLaptop | WorkLaptop | WorkDesktop deriving Eq
data MediaBackend = Cmus | Mpc deriving Eq
data MediaCmd = SongPause | SongNext | SongPrev deriving Eq

machine :: Machine
machine = case takeWhile (/= '.') . unsafePerformIO $ readFile "/etc/hostname" of
            "penguin"     -> HomeLaptop
            "bigpunisher" -> WorkDesktop
            "eviljeanius" -> WorkLaptop
            _             -> HomeLaptop

atWork :: Bool
atWork = case machine of
           WorkDesktop -> True
           WorkLaptop  -> True
           _           -> False

(=??) :: Query String -> String -> Query Bool
q =?? x = fmap (isInfixOf x) q

alt   = mod1Mask
mod   = mod4Mask
media = mod3Mask

setWallpaper :: String -> X ()
setWallpaper strWallpaper = spawn $ "feh --bg-fill $HOME/" ++ strWallpaper

myManageHook = fullscreenManageHook <+> manageSpawn <+> manageDocks <+> composeAll
   [ className =? "Gvim"    --> viewShift "2"
   , role =? "conversation" --> doFloat
   , isFullscreen           --> doFullFloat
   ]
     where viewShift = doF . liftM2 (.) W.greedyView W.shift
           role = stringProperty "WM_WINDOW_ROLE"

musicBackend :: MediaBackend
musicBackend
    | machine == HomeLaptop = Cmus
    | otherwise             = Mpc

musicRemote :: MediaCmd -> X ()
musicRemote
    | musicBackend == Cmus = cmus
    | musicBackend == Mpc  = mpc
    | otherwise            = error "Invalid music remote"
  where cmus SongNext  = safeSpawn "cmus-remote" ["--next"]
        cmus SongPrev  = safeSpawn "cmus-remote" ["--prev"]
        cmus SongPause = safeSpawn "cmus-remote" ["--pause"]
        mpc  SongNext  = safeSpawn "mpc" ["next"]
        mpc  SongPrev  = safeSpawn "mpc" ["prev"]
        mpc  SongPause = safeSpawn "mpc" ["toggLe"]

musicPrompt :: X ()
musicPrompt
    | musicBackend == Cmus = cmus
    | musicBackend == Mpc  = mpc
  where mpc = prompt $ \s ->
            "mpc clear; mpc search any \"" ++ s ++ "\" | mpc add; mpc play"
        cmus = prompt $ \s -> "cmus-remote -C 'live-filter " ++ s ++ "'"
        prompt f = inputPrompt defaultXPConfig "live-filter" ?+ (spawn . f)

musicProgram :: String
musicProgram
    | musicBackend == Cmus = "terminator -e 'cmus'"
    | musicBackend == Mpc  =
        "terminator --title='ncmpcpp' -e 'sleep 2 && ncmpcpp -s search-engine'"

musicTitle :: String
musicTitle
    | musicBackend == Cmus = "cmus"
    | musicBackend == Mpc  = "ncmpcpp"

-- StartupHook
myStartupHook :: X ()
myStartupHook = do setWMName "LG3D"
                   setWallpaper "Desktop/majora.png"
                   spawnOn "9" "pidgin"
                   spawn "nm-applet"
                   spawn "rescuetime"
                   if machine /= HomeLaptop
                      then spawn "mopidy"
                      else return ()
                   spawnOn "9" $ musicProgram
                   if machine == WorkLaptop
                      then spawn "xinput disable 11"
                      else return ()

myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor "#303030" "#909090" . pad
    , ppHidden          = dzenColor "#909090" "" . pad
    , ppHiddenNoWindows = const "" --dzenColor "#606060" "" . pad
    -- , ppLayout          = dzenColor "#909090" "" . pad
    , ppLayout          = const ""
    , ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip
    , ppTitle           = shorten 100
    , ppWsSep           = ""
    , ppSep             = "  "
    , ppOutput          = hPutStrLn h
    , ppExtras          = [ pcfbLogger ]
    }

myLayoutHook = avoidStruts $ tall
                         ||| Mirror tall
                         ||| noBorders (fullscreenFull Full)
  where tall = Tall 1 (3/100) (1/2)


trayHeight :: Int
trayHeight
    | machine == WorkLaptop = 24
    | otherwise             = 14

fontSize :: Int
fontSize
    | machine == WorkLaptop = 12
    | otherwise             = 6

main = do d <- spawnPipe
             $ mconcat
                [ "dzen2 "
                , "-p -ta l -h '"
                , show trayHeight
                , "' "
                , "-e 'onstart=lower' "
                , "-fn 'Bitstream Vera Sans-"
                , show fontSize
                , ":Bold'"
                ]

          t <- spawnPipe
             $ mconcat
                [ "trayer "
                , "--edge top --align right --SetDockType true "
                , "--SetPartialStrut true --expand false --widthtype pixel "
                , "--transparent true --tint 0 --alpha 0 --height "
                , show trayHeight
                ]

          c <- spawnPipe
             $ mconcat
                [ "conky -c $HOME/.xmonad/conky.rc | "
                , "dzen2 -x '700' -w '800' -h '" -- TODO: abstract these
                , show trayHeight
                , "' -ta 'r' -bg '#000000' "
                , "-fg '#FFFFFF' -y '0' -fn 'Bitstream Vera Sans-"
                , show fontSize
                , ":Bold'"
                ]

          spawn "xmodmap ~/.xmodmaprc"
          xmonad $ ewmh $ withUrgencyHook NoUrgencyHook $
            defaultConfig { modMask            = mod
                          , terminal           = "terminator"
                          , focusedBorderColor = "blue"
                          , startupHook        = myStartupHook
                          , manageHook         = myManageHook
                          , handleEventHook    = docksEventHook <+> fullscreenEventHook
                          , logHook            = myLogHook d
                          , layoutHook         = myLayoutHook

                          } `removeKeys`
                                [ (mod, xK_p)
                                , (mod .|. shiftMask, xK_p)
                                , (mod, xK_e)
                                , (mod, xK_r)
                                , (mod, xK_h)
                                , (mod, xK_l)
                                ]

                            `removeMouseBindings`
                                [ (mod, button1)
                                , (mod, button2)
                                , (mod, button3)
                                ]

                            `additionalMouseBindings`
                                [ ((alt, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
                                , ((alt, button2), windows . (W.shiftMaster .) . W.focusWindow)
                                , ((alt, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
                                ]

                            `additionalKeys` myKeys
                            `additionalKeys` pcfbKeys mod

myKeys =
        [ ((mod .|. shiftMask, xK_f),    runOrRaise "luakit" $ className =? "luakit")
        -- TODO: abstract this over atWork
        , ((mod, xK_f),                  runOrRaise "chromium-browser" $ className =? "Chromium-browser")
        , ((mod, xK_g),                  runOrRaise "gvim" $ className =? "Gvim")
        , ((mod .|. shiftMask, xK_q),    kill)
        , ((mod, xK_d),                  safeSpawnProg "synapse")
        , ((mod, xK_x),                  safeSpawnProg "terminator")
        , ((mod, xK_t),                  safeSpawnProg "thunar")
        , ((mod .|. controlMask, xK_l),  safeSpawn "cinnamon-screensaver-command" ["--lock"])
        , ((mod, xK_p),                  safeSpawnProg "scrot")
        , ((mod .|. shiftMask, xK_p),    spawn "sleep 0.2; scrot -s")
        , ((0, xF86XK_AudioRaiseVolume), safeSpawn "amixer" $ words "-q set Master 2dB+")
        , ((0, xF86XK_AudioLowerVolume), safeSpawn "amixer" $ words "-q set Master 2dB-")
        , ((media, xK_Left),             musicRemote SongPrev)
        , ((media, xK_Right),            musicRemote SongNext)
        , ((media, xK_Down),             musicRemote SongPause)
        , ((media, xK_m),                raise $ title =?? musicTitle)
        , ((media, xK_l),                musicPrompt)
        , ((mod, xK_l),                  nextScreen)
        , ((mod, xK_h),                  prevScreen)
        , ((mod .|. shiftMask, xK_h),    sendMessage Shrink)
        , ((mod .|. shiftMask, xK_l),    sendMessage Expand)
        ]

