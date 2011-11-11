#!/bin/bash

ROOT=`dirname $0`
PORT=":11"

kill_vnc() {
    vncserver -kill $PORT &> /dev/null
}

CMD=${1:-on}

case "$CMD" in
    on)
        kill_vnc
        vncserver $PORT -geometry 1024x768 &> /dev/null
        ;;
    off)
        kill_vnc
        ;;
    status)
        lsp Xvnc4
        ;;
    screen)
        DISPLAY=$PORT import -window root screen.png
        ;;
    *)
        DISPLAY=$PORT $ROOT/snap.py $@
        ;;
esac
