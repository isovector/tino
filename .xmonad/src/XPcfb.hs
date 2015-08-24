import Prelude hiding (mod)

import Control.Monad (when, liftM2)
import Data.Monoid (mconcat)
import Data.IORef
import Data.Pcfb
import System.IO
import System.IO.Unsafe

import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Core
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.Run
import qualified XMonad.StackSet as W

pcfbStart :: IO ()
pcfbStart = do
    proj <- readIORef mutProj
    writeStart proj Nothing
    writeIORef mutActive True

pcfbEnd :: IO ()
pcfbEnd = do
    writeEnd Nothing
    writeIORef mutActive False

withActiveProject :: IO () -> IO () -> IO ()
withActiveProject w wo = do
    ss <- todayStretches
    case activeProject ss of
      Just _  -> w
      Nothing -> wo

nothing :: IO ()
nothing = return ()

pcfbOpen :: IO ()
pcfbOpen = withActiveProject nothing pcfbStart

pcfbClose :: IO ()
pcfbClose = withActiveProject pcfbEnd nothing

pcfbPrompt :: X ()
pcfbPrompt = inputPrompt defaultXPConfig "new project" ?+ action'
  where action' p = liftIO $ do
            writeIORef mutProj p
            pcfbClose
            pcfbStart

mutProj :: IORef String
mutProj = unsafePerformIO $ newIORef ""
{-# NOINLINE mutProj #-}

mutActive :: IORef Bool
mutActive = unsafePerformIO $ newIORef False
{-# NOINLINE mutActive #-}

currentProject :: X String
currentProject = liftIO $ readIORef mutProj

{--
myKeys =
        [ ((mod, xK_f),                  runOrRaise "luakit" $ className =? "luakit")
        , ((mod, xK_g),                  runOrRaise "gvim" $ className =? "Gvim")
        , ((mod .|. shiftMask, xK_f),    runOrRaise "chromium-browser" $ className =? "Chromium-browser")
        , ((mod .|. shiftMask, xK_q),    kill)
        , ((mod, xK_d),                  safeSpawnProg "synapse")
        , ((mod, xK_x),                  safeSpawnProg "terminator")
        , ((mod, xK_t),                  safeSpawnProg "thunar")
        , ((mod, xK_p),                  safeSpawnProg "scrot")
        , ((mod .|. shiftMask, xK_p),    spawn "sleep 0.2; scrot -s")
        , ((0, xF86XK_AudioRaiseVolume), safeSpawn "amixer" $ words "-q set Master 2dB+")
        , ((0, xF86XK_AudioLowerVolume), safeSpawn "amixer" $ words "-q set Master 2dB-")
        , ((cmus, xK_Left),              safeSpawn "cmus-remote" ["--prev"])
        , ((cmus, xK_Right),             safeSpawn "cmus-remote" ["--next"])
        , ((cmus, xK_Down),              safeSpawn "cmus-remote" ["--pause"])
        , ((cmus, xK_m),                 raise $ title =?? "cmus")
        , ((cmus, xK_l),                 cmusPrompt)
        ]

--}

