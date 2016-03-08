# smblog-mode

Emacs major mode for samba log files.

## Features

- hilighting of log meta data messages
- easy navigation (`n` and `p`)
- filter by log level (`+` and `-`), files&functions (`f`)
- go to the source file (`RET` on any part of the log message)

## Installation

- place smblog.el somewhere in your `load-path`
- add `(require 'smblog)` in your init file
- add `(setq smblog-src-dir "/absolute/path/to/your/samba")`
  to use the go-to-source feature
- it should trigger on standard samba log file names,
  otherwise use `M-x smblog-mode-from-file-buffer` while visiting a log file
