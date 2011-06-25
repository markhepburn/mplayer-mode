MPlayer Mode:
=============

This is a simple minor-mode to control mplayer from within emacs. It was motivated when I was trying to take notes while watching a movie, continually pausing, skipping backwards a bit, etc.  Then at the finish I wished I'd been inserting timestamps as well.

So, this is the solution (writing it probably took longer than any time I'll ever save from using it, but we'll see).

To get started, either load the file directly, or add mplayer-mode directory to your load-path and call `(require 'mplayer-mode)`.

	; Mplayer-mode
	(add-to-list 'load-path "~/.emacs.d/mplayer-mode/")
	(require 'mplayer-mode)

Then, from the buffer you want to take notes in, call `M-x mplayer-find-file`.  This starts the movie playing, and you can investigate the documentation for `mplayer-mode` for the available commands.  It is possible to configure the timestamp format, default skip time, etc.
