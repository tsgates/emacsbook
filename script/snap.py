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
    dbg("[!] %s" % " ".join(opt))
    p = subprocess.Popen(opt)
    while True:
        time.sleep(0.1)
        res = xdotool_get_id(opt[0], p.pid)
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

def xdotool_get_id(exe, pid):
    exe_to_clss = {
        "emacs-terminal.sh": "terminal"
        }

    clss = exe_to_clss.get(exe, exe)
    
    return sh("xdotool search --pid %s --onlyvisible --classname %s" % (pid, clss))

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
                      dest="size", default=None)
    parser.add_option("-o", "--out",
                      help="output image file", 
                      dest="out", default="out.png")
    parser.add_option("-b", "--non-batch",
                      help="non batch processing", action="store_false",
                      dest="batch", default=True)
    parser.add_option("-a", "--args",
                      help="command arguments",
                      dest="args", default="")
    parser.add_option("-e", "--exe",
                      help="exe commands", 
                      dest="exe", default="emacs")
    parser.add_option("-c", "--clean",
                      help="clean emacs", action="store_true",
                      dest="clean", default=False)    
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

    cmdargs = opts.args.split()
    if opts.size:
        cmdargs.append("--geometry=" + opts.size + "+0+0")
    if opts.clean:
        cmdargs.append("--no-init")
        
    (res, proc) = run_emacs(opts.exe, *cmdargs)
    
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

    snapshot(res, opts.out)
    proc.kill()

    # resizing
    # img = Image.open(filename).resize( (400,400) )
    # img = img.resize( (200,200), Image.ANTIALIAS)

    if not opts.batch:
        os.system("DISPLAY=:0 gnome-open %s" % opts.out)
