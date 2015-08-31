module XPcfb ( pcfbOpen
             , pcfbClose
             , pcfbPrompt
             , currentProject
             , pcfbKeys
             , pcfbLogger
             ) where

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
    hello Nothing
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

pcfbLogger :: X (Maybe String)
pcfbLogger = liftIO $ do
    proj   <- readIORef mutProj
    active <- readIORef mutActive
    return $ if active
       then Just $ "WORKING: " ++ proj
       else Nothing

pcfbKeys :: ButtonMask -> [((ButtonMask, KeySym), X ())]
pcfbKeys mod =
        [ ((mod, xK_bracketleft),   liftIO $ pcfbOpen)
        , ((mod, xK_bracketright),  liftIO $ pcfbClose)
        , ((mod, xK_c),             pcfbPrompt)
        ]

