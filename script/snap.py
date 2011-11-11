#!/usr/bin/python

import os
import sys
import time
import optparse
import commands
import subprocess

def dbg(msg):
    print msg

def sh(cmd):
    rtn = commands.getoutput(cmd).strip()
    dbg("[!] %s %s" % (cmd, ("=> " + rtn) if rtn != "" else ""))
    return rtn

def run_emacs(*opt):
    p = subprocess.Popen(["emacs"] + list(opt))
    while True:
        time.sleep(0.1)
        res = xdotool_get_id(p.pid)
        if res != "" and "Error" not in res:
            time.sleep(1)
            return (res, p)
    # unreachable

def xdotool_win_size(res, width, height):
    sh("xdotool windowsize %s %s %s" % (emacs_id, width, height))
    time.sleep(0.1)

def xdotool_win_move(res, x, y):
    sh("xdotool windowmove %s %s %s" % (emacs_id, x, y))

def xdotool_get_id(pid):
    return sh("xdotool search --pid %s --onlyvisible --name emacs" % pid)

def send_key(res, keys):
    sh("xdotool key --window %s --clearmodifiers %s" % (res, keys))

def send_type(res, keys):
    sh("xdotool type --window %s \"%s\"" % (res, keys))

def snapshot(res, img):
    sh("import -w %s %s" % (res, img))

#
# M-x a C-a abcd RET
#
# split by space
#  M- => alt+
#  C- => ctrl+
#  RET => Return
#

if __name__ == '__main__':
    # gtk geometry: row x col + x + y
    parser = optparse.OptionParser(__doc__.strip() if __doc__ else "")
    parser.add_option("-s", "--size",
                      help="image (width x height)", 
                      dest="size", default="80x25")
    parser.add_option("-i", "--img",
                      help="image file", 
                      dest="img", default="out.png")
    (opts, args) = parser.parse_args()
    
    # modifier map
    mmap = {"M-" : "alt+"  ,
            "C-" : "ctrl+" ,
            "S-" : "super+",
            "RET": "Return"}

    (res, proc) = run_emacs("--no-init", "--geometry=" + opts.size + "+0+0")
    
    dbg("[!] found emacs-id: %s" % res)
    time.sleep(1)
    
    # feed key
    for cmd in args:
        # if key
        if any(m in cmd for m in mmap):
            for m in mmap:
                cmd = cmd.replace(m, mmap[m])
    
            send_key(res, cmd)
            continue
        
        # if type
        send_type(res, cmd)

    snapshot(res, opts.img)
    proc.kill()