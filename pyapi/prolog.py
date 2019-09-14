import fcntl
import os
import sys


def pipe_into_stdin_block(facto, popen):
    fd = popen.stdout.fileno()
    fl = fcntl.fcntl(fd, fcntl.F_GETFL)
    # clearing up NONBLOCK flag
    fcntl.fcntl(fd, fcntl.F_SETFL, fl & (not os.O_NONBLOCK))
    print(facto)
    print >> popen.stdin, facto
    popen.stdin.flush()
    r = ""
    try:
        while True:
            r += popen.stdout.readline().strip()
    except:
        return r.strip()
    return r.strip()


def pipe_into_stdin(facto, popen):
    print(facto)
    print >> popen.stdin, facto
    popen.stdin.flush()
    r = ""
    try:
        while True:
            r += popen.stdout.readline().strip()
    except:
        return r.strip()
    return r.strip()


def query(predicado):
    # test
    return ""
    # return prolog.query(predicado)
