module Profiles ( MediaBackend (..)
                , Machine (..)
                , MediaCmd (..)
                , machine
                , atWork
                , musicBackend
                , musicRemote
                , musicPrompt
                , musicProgram
                , musicTitle
                , browserProgram
                , browserTitle
                , trayHeight
                , fontSize
                ) where

import Prelude hiding (mod)

import Control.Monad (liftM2)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import XMonad
import XMonad.Core
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.Run

data MediaBackend = Cmus       | Mpc                      deriving Eq
data Machine      = HomeLaptop | WorkLaptop | WorkDesktop deriving Eq
data MediaCmd     = SongPause  | SongNext   | SongPrev    deriving Eq

machine :: Machine
machine = case takeWhile predicate . unsafePerformIO $ readFile "/etc/hostname" of
            "penguin"           -> HomeLaptop
            "bigpunisher"       -> WorkDesktop
            "maguirea-glaptop0" -> WorkLaptop
            _                   -> HomeLaptop
    where predicate = liftM2 (&&) ('.' /=) ('\n' /=)

atWork :: Bool
atWork = case machine of
           WorkDesktop -> True
           WorkLaptop  -> True
           _           -> False

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
        mpc  SongPause = safeSpawn "mpc" ["toggle"]

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

browserProgram :: String
browserProgram
    | atWork    = "google-chrome"
    | otherwise = "chromium-browser"

browserTitle :: String
browserTitle
    | atWork    = "Google-chrome"
    | otherwise = "Chromium-browser"

trayHeight :: Int
trayHeight
    | machine == WorkLaptop = 24
    | otherwise             = 14

fontSize :: Int
fontSize
    | machine == WorkLaptop = 12
    | otherwise             = 6

