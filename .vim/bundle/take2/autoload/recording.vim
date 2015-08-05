python << endpython
import vim
from os import unlink, write, close
from shutil import rmtree
from tempfile import mkstemp, mkdtemp
import xmlrpclib, socket

global accio, accioProc, gTimer, gIsActive, gUpdatesToSend, gIdleCount, IS_IDLE_COUNT, RECORD_DURATION

gTimer = None
gIsActive = False
gUpdatesToSend = 0
gIdleCount = 0

RECORD_DURATION = 5.0
IS_IDLE_COUNT = 12

accio = xmlrpclib.ServerProxy("http://localhost:7432/")
socket.setdefaulttimeout(int(RECORD_DURATION) - 1)

def make_tempfile(content=None):
    """ Creates a temporary file and returns the path. """
    fd, tmpfile = mkstemp()

    if content:
        write(fd, content)

    close(fd)
    return tmpfile


def delete_tempfile(path):
    """ Deletes a temporary file """
    try:
        unlink(path)
    except:
        pass


def get_window_range():
    """
    Gets the currently visible lines in the window.
    """
    cursor = vim.current.window.cursor
    localCursor = int(vim.eval("winline()"))
    topOfWindow = cursor[0] - localCursor + 1
    return (topOfWindow, topOfWindow + vim.current.window.height)


def send_to_daemon(filename, buffer, window_range):
    """
    Send all data to the daemon, piping the current buffer's content into
    stdin.
    """
    from subprocess import Popen, PIPE
    from os import getpid

    tmp = make_tempfile(buffer)
    try:
        accio.snapshot(
            filename,
            window_range[0],
            window_range[1],
            tmp)
    except:
        pass
    delete_tempfile(tmp)


def collect_metrics():
    """
    Collect metrics and send it to the daemon.
    """
    from threading  import Thread

    global gUpdatesToSend
    if gUpdatesToSend > 0:
        t = Thread(
            target = send_to_daemon,
            args = (
                vim.current.buffer.name,
                "\n".join(vim.current.buffer[:]) + "\n",
                get_window_range()
            )
        )

        t.daemon = True
        t.start()

        gUpdatesToSend -= 1



# All code below this is just magic to get the above working

def scheduleNextTimer():
    """
    Set a callback for the future.
    """
    from threading import Timer
    global RECORD_DURATION

    def timerThread():
        """
        Helper function to reschedule the timer
        """
        global gIdleCount, gUpdatesToSend
        if isActive():
            gUpdatesToSend += 1
        gIdleCount += 1
        scheduleNextTimer()

    global gTimer
    gTimer = Timer(RECORD_DURATION, timerThread)
    gTimer.start()

def isActive():
    """
    Check whether or not there has been input lately.
    """
    global gIdleCount, IS_IDLE_COUNT
    return gIdleCount < IS_IDLE_COUNT

def setActive():
    """
    Called by vim to inform us the user has moved the cursor.
    """
    global gIdleCount
    gIdleCount = 0
    collect_metrics()

def setIdle():
    """
    Called by vim to inform us we lost focus.
    """
    global gIdleCount, gUpdatesToSend, IS_IDLE_COUNT
    gIdleCount = IS_IDLE_COUNT
    gUpdatesToSend = 0

endpython

function! recording#onActivity()
    python << endpython
setActive()
endpython
endfunction

function! recording#onIdle()
    python << endpython
setIdle()
endpython
endfunction

function! recording#onEnter()
    python << endpython
from subprocess import Popen, PIPE
global accioProc
accioProc = Popen(['accio'], stdout=PIPE, stderr=PIPE)
scheduleNextTimer()
endpython
endfunction

function! recording#onExit()
    python << endpython
global accioProc
accioProc.terminate()
gTimer.cancel()
gTimer = None
endpython
endfunction

" __END__
" vim: foldmethod=marker
