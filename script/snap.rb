#!/usr/bin/env ruby1.9.1

require 'fileutils'

# xdotool key --window $e --clearmodifiers alt+x
# xdotool type "./p projects/dist-db/main.l -go 1"
# xdotool key Return
# xdotool key ctrl+shift+o
# xdotool type "./p projects/dist-db/main.l -go 2"
# xdotool key Return
# xdotool key ctrl+shift+o
# xdotool type "./p projects/dist-db/controller.l -go"
# xdotool key Return

def run_emacs(opt="")
  pid = Process.spawn "emacs #{opt}"
  # XXX. wait: need a way to wait emacs intelligently
  sleep(1.0)
  return xdotool_get_id(pid)
end

def xdotool_get_id(pid)
  return `xdotool search --pid #{pid} --onlyvisible`.strip
end

def send_key(res, keys)
  # translate emacs keys to xdotool
  # M- => alt+
  # C- => ctrl+
  `xdotool key --window #{res} --clearmodifiers alt+x`
end

def send_type(res, keys)
  `xdotool type --window #{res} #{keys}`
end

# wait until calm down
emacs_id = run_emacs "--no-init"

# send_key
send_key(emacs_id, "")
send_type(emacs_id, "test")

# key    =
# width  =
# height =

`xdotool windowmove #{emacs_id} 100 0`
`xdotool windowsize #{emacs_id} 400 300`

# take a snapshot
`import -w #{emacs_id} test.png`
