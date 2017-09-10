module Log
       ( -- * XMonad Logging feature
         -- $LOG

         -- ** Setting up the logger
         setupLogger

         -- ** Working with the logger
       , debugX
       , infoX
       , noticeX
       , warningX
       , errorX
       , criticalX
       , alertX
       , emergencyX

       -- ** Abort with error logging.
       , abortX
       , abortX'
       , Priority (..)
       )
       where

import System.IO (stderr)
import System.FilePath ((</>))

import System.IO.Unsafe (unsafePerformIO) -- used when aborting

import System.Log.Logger (Priority(..), logM, setHandlers, updateGlobalLogger, rootLoggerName, setLevel)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Formatter (simpleLogFormatter)

import Control.Monad.State


-- $LOG

-- Logging support for XMonad, that will log to 'stderr' and a file. Everything
-- that is written to 'stderr' is placed in @~/.xsession-errors@, however this
-- might not be optimal, as on some systems quite a few applications tend to be
-- chatty and thus the XMonad specific messages may be hard to find. To solve
-- this, everything is also written into an XMonad specific log such that it is
-- easy to locate.
--
-- The level of "chattyness" can be controlled by setting the 'Priority' in the
-- XMonad configuration. Setting the priority to 'WARNING' will show any
-- messages with that priority and above.
-- TODO: Fix this documentation.
-- * Add example of setting up default config for different logging levels.


-- | Setup a logger in @dir@/xmonad.log and on stderr. 'WARNING's and above will
-- be written to 'stderr', but only @lowestPriority@ will be written to the
-- log file.
setupLogger :: MonadIO m => Priority -> FilePath -> m ()
setupLogger lowestPriority dir = liftIO $
  do fileH   <- fileHandler (dir </> logFile) lowestPriority
     streamH <- streamHandler stderr          WARNING
     updateGlobalLogger rootLoggerName $
       setLevel DEBUG . -- We use individual priorities above.
       setHandlers (map (flip setFormatter $ format) [streamH, fileH])
  where
    format  = simpleLogFormatter "$time, $loggername [$prio]: $msg"
    logFile = "xmonad.log"


-- | Main log function used by the specialised loggers below.
logX :: MonadIO m => Priority -> String -> String -> m ()
logX prio name msg =
  liftIO $ logM name prio msg

-- | Logging with various importance. Importance goes from DEBUG through
-- EMERGENCY, with EMERGENCY being the most important.
debugX, infoX, noticeX, warningX, errorX, criticalX, alertX, emergencyX :: MonadIO m => String -> String -> m ()
debugX     = logX DEBUG     -- Debug messages
infoX      = logX INFO      -- Information
noticeX    = logX NOTICE    -- Normal runtime conditions
warningX   = logX WARNING   -- General Warnings
errorX     = logX ERROR     -- General Errors
criticalX  = logX CRITICAL  -- Severe situations
alertX     = logX ALERT     -- Take immediate action
emergencyX = logX EMERGENCY -- System is unusable

-- | Abort execution, yielding a critical log entry and an error.
abortX :: MonadIO m => String -> String -> m a
abortX name msg =
  do criticalX name msg
     error $ "xmonad: " ++ name ++ ": " ++ msg

-- | Abort execution outside MonadIO.
abortX' :: String -> String -> a
abortX' name msg =
  -- force execution of abortX
  id $! unsafePerformIO $ abortX name msg
