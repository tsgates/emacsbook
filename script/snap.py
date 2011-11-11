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
        if res != "" \
                and "Error" not in res \
                and "\n" not in res:
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
    parser.add_option("-b", "--batch",
                      help="batch processing", action="store_true",
                      dest="batch", default=False)
    # XXX. implement later
    parser.add_option("-r", "--restart",
                      help="force restart", action="store_true",
                      dest="restart", default=False)
    (opts, args) = parser.parse_args()

    # modifier map
    mmap = {
        "M-" : "alt+"  ,
        "C-" : "ctrl+" ,
        "S-" : "super+",
        "RET": "Return",
        "\n" : "Return", 
        " "  : "space", 
        "?"  : "shift+question", 
        "!"  : "shift+exclam", 
        ","  : "comma", 
        "."  : "period", 
        ";"  : "shift+semicolon", 
        ":"  : "shift+colon", 
        '"'  : "shift+2", 
        "$"  : "shift+4", 
        "%"  : "shift+5", 
        "&"  : "shift+6", 
        "/"  : "shift+7", 
        "("  : "shift+8", 
        ")"  : "shift+9", 
        "="  : "shift+0", 
        "^"  : "dead_circumflex+dead_circumflex", 
        "*"  : "shift+asterisk", 
        "#"  : "numbersign", 
        "'"  : "shift+apostrophe", 
        "-"  : "minus", 
        "_"  : "shift+underscore", 
        "<"  : "less", 
        ">"  : "shift+greater", 
        "\t" : "Tab", 
        "\b" : "BackSpace", 
        }

    (res, proc) = run_emacs("--no-init", "--geometry=" + opts.size + "+0+0")
    
    dbg("[!] found emacs-id: %s" % res)
    time.sleep(1)
    
    # feed key
    for cmd in args:
        # if type
        if cmd.startswith('"') and cmd.endswith('"'):
            send_type(res, cmd[1:-1])
            continue
        
        # if key
        for m in mmap:
            cmd = cmd.replace(m, mmap[m])
    
        send_key(res, cmd)

    snapshot(res, opts.img)
    proc.kill()

    if not opts.batch:
        os.system("DISPLAY=:0 gnome-open %s" % opts.img)