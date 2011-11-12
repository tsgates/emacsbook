#!/bin/bash
gnome-terminal -e "emacs -nw $@"

kill_emacs() {
  killall emacs &> /dev/null
  exit 1
}

trap kill_emacs INT TERM

while true; do
    sleep 1
done