;;; mplayer-mode.el --- control mplayer, facilitating transcription and note-taking.

;; Copyright (C) 2011 Mark Hepburn
;; Copyright (C) 2015 Karl M. Hegbloom

;; Author: Mark Hepburn (mark.hepburn@gmail.com)
;; Compatibility: Emacs20, Emacs21, Emacs22, Emacs23, Emacs24

;; Improvements by: Karl M. Hegbloom (karl.hegbloom@gmail.com)
;;
;; 2015-11-12 Added:
;;
;; `mplayer-find-file-at-point'            M-x mplayer-find-file-at-point
;;
;; `mplayer-insert-position-and-timestamp' C-x SPC h
;;
;; `mplayer-seek-position-at-point'        C-x SPC g
;;
;; `mplayer-file-start-offset' buffer-local in seconds since midnight.
;;
;; When I do a transcription, I put the audio file's name at the top
;; of the file, and also a heading, like:
;;
;; # -*- mode: text; mplayer-file-start-offset: 40430; -*-
;;
;; ... so that the time stamps reflect the actual time of day when the
;; recording was made. Obviously that only works when you have that
;; information, as for a court recording.
;;
;; Suggestions for using this mode:
;;
;; In my .emacs, I have:
;;
;; (autoload 'mplayer-find-file "mplayer-mode" "Control mplayer from within Emacs")
;; (autoload 'mplayer-find-file-at-point "mplayer-mode" "Control mplayer from within Emacs")
;; (require 'mplayer-mode)
;; (fset 'mplayer-jump-to-start-of-this-block
;;    [?\C-\M-r ?\( ?\[ ?0 ?- ?9 ?\] right ?\C-x ?  ?g ?\C-u ?\C- ])
;; (define-key text-mode-map (kbd "<f11>") #'mplayer-jump-to-start-of-this-block)
;;
;; I would have created an interactive function for the jump to start
;; of block, except that the format of a "position" is configurable,
;; so I could not hard-code the isearch backward regexp expression
;; this macro uses.
;;
;; When I open a new transcription file, I insert the mode heading,
;; and then use C-u M-! to run:
;;
;; lltags -S 2011-09-09-10-25-00_10-30-12_111905405_Audio_W48_Scheduling_Conference.flac
;;
;; ... and it inserts the file's name and the flac metadata, which
;; I've set for each file, giving casename, courtroom, casenumber,
;; date, hearingtype, start, and end times, most of which is also
;; encoded in the file-name, which is designed so they sort
;; chronographically. I delete the : from the end of the file-name it
;; prints, put # at the start of each line in case I want to strip
;; that header from a munged copy used for pretty presentation,
;; perhaps run through a script and LaTeX'd, and format the heading
;; nicely to make it readable, with = aligned...
;;
;; Now I can put the cursor on the file name, and use:
;;    M-x mplayer-find-file-at-point to begin playback.
;;
;; I determine and enter a key at the top, giving Judge, Victim
;; Advocate, Prosecutor, Defender, and Defendant, but during the part
;; where they announce their appearances at the start of the
;; hearing. To begin with, I type:
;;
;; C-x SPC SPC to pause the audio, then at the bottom of the file, the
;; start of the new transcript text, I enter:
;;
;; (0.0) F11    using my macro to seek to the start of the file.

;; And then C-x SPC i to insert the timestamp for the start of the
;; hearing, and check to be sure it's right, properly adjusted to the
;; correct time of day for the hearing start.
;;
;; Any time there's a new speaker, I use C-x SPC h to insert a
;; location and a timestamp, then I enter the person's designation,
;; eg. Judge: blah blah what she said here.  Any time I need to jump
;; back, to the beginning of the utterance I can push F11. I can
;; insert a position using C-x SPC t, to set points where I can jump
;; back to, to hear it again. To get the right setting for the start
;; of an utterance, I use C-x SPC t, then listen to it, and adjust the
;; number manually to hit the desired spot in the audio file. With it
;; paused and F11 pushed to seek to the point indicated by the
;; position marker, I then use C-x SPC i to insert the
;; timestamp. Sometimes there's enough time to listen to the tail of
;; the utterance, with C-x SPC pressed, and my finger poised above the
;; h key...
;;
;; This system works very well and makes it easy to jump back to a
;; spcecific point in the recording.
;;
;; The flac audio format is the only one that this is likely to work
;; well with aside from wav, since a flac file has an embedded seek
;; point index. If you try this with an ogg or an mp3, it might not
;; jump to exactly the same spot each time due to variable bitrate
;; encoding. With vbr encoded audio, the amount of file space required
;; to represent a given time length is variable throughout the file,
;; so there's not a deterministic arithmatic to compute an exact time
;; offset within the file. That's why flac have seek point indexes in
;; them!


;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Owes a lot in initial idea to the emacs video editor gneve
;; (http://www.1010.co.uk/gneve.html).  This mode controls mplayer
;; directly, using its slave-mode (see
;; http://www.mplayerhq.hu/DOCS/tech/slave.txt), which accepts
;; commands on stdin.  The original motivation was to facilitate note
;; taking from videos; hence it is possible to pause, skip backwards
;; and forwards, and insert a timestamp of the current position.

;;; Use:

;;; Install:

;; Put something similar to the following in your ~/.emacs to use this file:
;;
;; (load "~/path/to/mplayer-mode.el")
;;

;;; Dependency:

;; mplayer

;;; TODO:
;; - Proper org-mode integration would probably be nice (eg, a link to the file)
;; - Error handling and clean-up

;;; Code:

(defgroup mplayer nil
  "Group used to store various mplayer-mode variables.")


(defcustom mplayer-executable "mplayer"
  "Name or path to the mplayer executable."
  :type 'file
  :group 'mplayer)

(defvar mplayer-mode-map nil
  "Local keymap for mplayer-mode")

;;; This prefix is chosen for ergonomic accessibility; it does ignore
;;; the recomendations about C-x being for global combinations, etc,
;;; so change if it's inconvenient.
(defcustom mplayer-prefix-command "\C-x "
  "The prefix for all mplayer minor-mode commands. Default C-x SPC."
  :type 'key-sequence
  :group 'mplayer)

(defcustom mplayer-default-seek-step 10
  "The number of seconds that the skip command will use."
  :type 'integer
  :group 'mplayer)

(defcustom mplayer-default-speed-step 10
  "The increase/decrease of playback speed that the faster/slower commands will use (percent of standard playback speed)."
  :type 'integer
  :group 'mplayer)

(defcustom mplayer-osd-level 3
  "OSD level used by mplayer.  3 (the default) means position/length."
  :type 'integer
  :group 'mplayer)

(defcustom mplayer-position-stamp-format "(%s)"
  "Format used for inserting position stamps.
It must contain \%s since the position is given as a string where
this is called."
  :type 'string
  :group 'mplayer)

(defcustom mplayer-timestamp-format "[%H:%M:%S.%1N] "
  "Format used for inserting timestamps."
  :type 'string
  :group 'mplayer)


;;; Utilities:

(defun mplayer--send (cmd)
  (process-send-string mplayer-process (concat cmd "\n")))

(defun mplayer--parse-seconds (seconds)
  (cond
   ((null seconds) mplayer-default-seek-step)
   ((numberp seconds) seconds)
   ((listp seconds)
    (* mplayer-default-seek-step (log (abs (car seconds)) 4)))))

(defun mplayer--parse-speedstep (speedstep)
  (cond
   ((null speedstep) (/ mplayer-default-speed-step 100.0))
   ((numberp speedstep) (/ speedstep 100.0))
   ((listp speedstep)
    (/ (* mplayer-default-speed-step (+ 1 (log (abs (car speedstep)) 4))) 100.0))))

(defvar mplayer-file-start-offset 0
  "Buffer local wall clock offset for start of file, in seconds.")
(make-variable-buffer-local 'mplayer-file-start-offset)
(put 'mplayer-file-start-offset 'safe-local-variable #'identity)

(defun mplayer--format-time (time)
  "Return a formatted time string, using the format string
`mplayer-timestamp-format'.  The argument is in seconds, and
can be an integer or a string. If the buffer-local variable
`mplayer-file-start-offset' is set, that offset is added to
the argument. Its value must be specified in seconds since
midnight."
  (message "format-time: %s" time)
  (if (stringp time)
      (setq time (string-to-number time)))
  (if (stringp mplayer-file-start-offset)
      (setq mplayer-file-start-offset
            (string-to-number mplayer-file-start-offset)))
  (setq time (+ time mplayer-file-start-offset))
  (message "time to format: %.1f" time)
  ;; All this truncating and rounding is needed because floating point
  ;; numbers are not exact, and multiplying by 1000000 amplifies the
  ;; error, causing the timestamp to end with .6 rather than .7, etc.
  ;; Multiplying by 10, rounding, then dividing by 10.0 gets it right.
  (let ((sec (truncate time))
        (usec (truncate (* 1000000
                           (/ (round (* 10
                                        (- time
                                           (truncate time))))
                              10.0)))))
    (format-time-string
     mplayer-timestamp-format `(0 ,(truncate time) ,usec) t)))


;;; Interactive Commands:

(defun mplayer-find-file (filename)
  "Entry point to this mode.  Starts playing the file using
mplayer, and enables some keybindings to support it; see the
documentation for `mplayer-mode' for available bindings."
  (interactive "fOpen recording file: ")
  (set (make-local-variable 'mplayer--osd-enabled) nil)
  (set (make-local-variable 'mplayer-process-buffer) (generate-new-buffer "*mplayer*"))
  (set (make-local-variable 'mplayer-process)
       (start-process "mplayer" mplayer-process-buffer
                      mplayer-executable
                      "-quiet" "-slave"
                      filename))
  (mplayer-mode t))

(defun mplayer-find-file-at-point ()
  "Start mplayer on the file name at point. Optionally provide
  base director where the audio file is expected to be located."
  (interactive)
  (let ((filename (thing-at-point 'filename)))
    (mplayer-find-file filename)))

(defun mplayer-toggle-pause ()
  "Pause or play the currently-open recording."
  (interactive)
  (mplayer--send "pause"))

(defun mplayer-seek-forward (seconds)
  "Skip forward in the recording.  By default this is
`mplayer-default-seek-step' seconds; it can also be specified as
a numeric prefix arg, or plain prefix args act as a
successive (linear) multipliers of `mplayer-default-seek-step'."
  (interactive "P")
  (let ((seconds (mplayer--parse-seconds seconds)))
    (mplayer--send (format "seek %d 0" seconds))))

(defun mplayer-seek-backward (seconds)
  "Skip backward in the recording.  By default this is
`mplayer-default-seek-step' seconds; it can also be specified as
a numeric prefix arg, or plain prefix args act as a
successive (linear) multipliers of `mplayer-default-seek-step'."
  (interactive "P")
  (let ((seconds (- (mplayer--parse-seconds seconds))))
    (mplayer--send (format "seek %d 0" seconds))))

(defun mplayer-faster (speedstep)
  "Increase playback speed. By default by `mplayer-default-speed-step' percentage points; it can also be set with a numeric prefix arg, or plain prefix args acts as successive multipliers (2,3,4...) of `mplayer-default-speed-step'"
  (interactive "P")
  (let ((speedstep (mplayer--parse-speedstep speedstep)))
    (mplayer--send (format "speed_incr %.2f" speedstep))))

(defun mplayer-slower (speedstep)
  "Decreaser playback speed. By default by `mplayer-default-speed-step' percentage points; it can also be set with a numeric prefix arg, or plain prefix args acts as successive multipliers (2,3,4...) of `mplayer-default-speed-step'"
  (interactive "P")
  (let ((speedstep (mplayer--parse-speedstep speedstep)))
    (mplayer--send (format "speed_incr -%.2f" speedstep))))

(defun mplayer-reset-speed ()
  "Reset playback speed."
  (interactive)
  (mplayer--send "speed_set 1"))

(defun mplayer-toggle-osd ()
  "Toggle on-screen display on or off.  See `mplayer-osd-level'
for the type of display."
  (interactive)
  (if mplayer--osd-enabled
      (mplayer--send "osd")
    (mplayer--send (format "osd %d" mplayer-osd-level)))
  (setq mplayer--osd-enabled (not mplayer--osd-enabled)))

(defun mplayer-insert-timestamp ()
  "Insert a time-stamp of the current recording position in the
buffer.  See `mplayer-timestamp-format' for the insertion
format."
  (interactive)
  (let (time)
    (set-process-filter
     mplayer-process
     ;; wait for output, process, and remove filter:
     (lambda (process output)
       (message "process: %s output: %s" process output)
       (string-match "^ANS_TIME_POSITION=\\(.*\\)$" output)
       (setq time (match-string 1 output))
       (if time
           (insert (mplayer--format-time time))
         (message "MPlayer: couldn't detect current time."))
       (set-process-filter mplayer-process nil)))
    ;; Then send the command:
    (mplayer--send "get_time_pos")))

(defun mplayer-insert-position ()
  "Insert the current recording position in seconds,
into the buffer."
  (interactive)
  (let (time)
    (set-process-filter
     mplayer-process
     ;; wait for output, process, and remove filter:
     (lambda (process output)
       (message "process: %s output: %s" process output)
       (string-match "^ANS_TIME_POSITION=\\(.*\\)$" output)
       (setq time (match-string 1 output))
       (if time
           (insert (format mplayer-position-stamp-format time))
         (message "MPlayer: couldn't detect current time."))
       (set-process-filter mplayer-process nil)))
    ;; Then send the command:
    (mplayer--send "get_time_pos")))

(defun mplayer-insert-position-and-timestamp ()
  (interactive)
  (let (time)
    (set-process-filter
     mplayer-process
     ;; wait for output, process, and remove filter:
     (lambda (process output)
       (message "process: %s output: %s" process output)
       (string-match "^ANS_TIME_POSITION=\\(.*\\)$" output)
       (setq time (match-string 1 output))
       (if (not time)
           (message "MPlayer: couldn't detect current time.")
         (insert (format mplayer-position-stamp-format time))
         (insert (mplayer--format-time time)))
       (set-process-filter mplayer-process nil)))
    ;; Then send the command:
    (mplayer--send "get_time_pos")))

(defun mplayer-seek-position (position)
  "Seek to some place in the recording."
  ;; (interactive "P")
  (interactive "nEnter seek position: ")
  ;; (message "Seeking to position: %d" position)
    (mplayer--send (format "seek %.1f 2" position)))

;; (bounds-of-thing-at-point 'mplayer-pos)34.9
;; (thing-at-point 'mplayer-pos)34.9

(put 'mplayer-pos 'bounds-of-thing-at-point
     (lambda ()
       (let ((thing (thing-at-point-looking-at "[0-9]+\\.[0-9]" 6)))
         (if thing
             (let ((beginning (match-beginning 0))
                   (end (match-end 0)))
               (cons beginning end))))))

(put 'mplayer-pos 'thing-at-point
     (lambda ()
       (let ((boundary-pair (bounds-of-thing-at-point 'mplayer-pos)))
         (if boundary-pair
             (string-to-number
              (buffer-substring-no-properties
               (car boundary-pair) (cdr boundary-pair)))))))


(defun mplayer-seek-position-at-point ()
  "Seek to the position represented by the number at point."
  (interactive)
  (let ((pos (thing-at-point 'mplayer-pos)))
    (message "Seeking to position: %.1f" pos)
    (mplayer--send (format "seek %.1f 2" pos))))

(defun mplayer-quit-mplayer ()
  "Quit mplayer and exit this mode."
  (interactive)
  (mplayer--send "quit")
  (set-process-filter
   mplayer-process
   (lambda (process output)
     (kill-buffer mplayer-process-buffer)))
  (mplayer-mode nil))

;;; Mode setup:

(unless mplayer-mode-map
  (setq mplayer-mode-map (make-sparse-keymap)))

(let ((map (make-sparse-keymap)))
  (define-key map (kbd "SPC")     'mplayer-toggle-pause)
  (define-key map (kbd "<right>") 'mplayer-seek-forward)
  (define-key map (kbd "<left>")  'mplayer-seek-backward)
  (define-key map (kbd "f")       'mplayer-faster)
  (define-key map (kbd "s")       'mplayer-slower)
  (define-key map (kbd "r")       'mplayer-reset-speed)
  (define-key map (kbd "p")       'mplayer-seek-position)
  (define-key map (kbd "g")       'mplayer-seek-position-at-point)
  (define-key map (kbd "t")       'mplayer-insert-position)
  (define-key map (kbd "d")       'mplayer-toggle-osd)
  (define-key map (kbd "i")       'mplayer-insert-timestamp)
  (define-key map (kbd "h")       'mplayer-insert-position-and-timestamp)
  (define-key map (kbd "q")       'mplayer-quit-mplayer)

  (define-key mplayer-mode-map mplayer-prefix-command map))

;;; This is putting a keybinding into somebody else's keymap... But we
;;; want that only when this mode is loaded, so why not? What are the
;;; odds of it clashing with another keybinding really?
(define-key text-mode-map (kbd "C-x SPC C-x C-f") 'mplayer-find-file-at-point)

(define-minor-mode mplayer-mode
  "Control mplayer from within Emacs.  Mainly intended for
transcription purposes, so commands exist to pause, seek, set playback speed, and
insert the current time as a timestamp.  This mode should not be
invoked directly; see `mplayer-find-file' and
`mplayer-quit-mplayer' for the entry and exit points.

Key bindings:
\\{mplayer-mode-map}"
  nil                                   ; initial value
  " MPlayer"                            ; mode-line string
  mplayer-mode-map)

(provide 'mplayer-mode)
