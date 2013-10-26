;;; mplayer-mode.el --- control mplayer, facilitating transcription and note-taking.

;; Copyright (C) 2011 Mark Hepburn

;; Author: Mark Hepburn (mark.hepburn@gmail.com)
;; Compatibility: Emacs20, Emacs21, Emacs22, Emacs23

;; This file is not part of GNU Emacs.

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

(defcustom mplayer-timestamp-format "%H:%M:%S"
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

(defun mplayer--format-time (time)
  "Return a formatted time string, using the format string
`mplayer-timestamp-format'.  The argument is in seconds, and
can be an integer or a string."
  (message "format-time: %s" time)
  (if (stringp time)
      (setq time (round (string-to-number time))))
  (message "time to format: %s" time)
  (format-time-string mplayer-timestamp-format `(0 ,time 0) t))

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
           (insert time)
         (message "MPlayer: couldn't detect current time."))
       (set-process-filter mplayer-process nil)))
    ;; Then send the command:
    (mplayer--send "get_time_pos")))

(defun mplayer-seek-position (position)
  "Seek to some place in the recording."
  ;; (interactive "P")
  (interactive "nEnter seek position: ")
  ;; (message "Seeking to position: %n" position)
    (mplayer--send (format "seek %d 2" position)))

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
  ;; (define-key map (kbd "f")       'mplayer-find-file)
  (define-key map (kbd "SPC")     'mplayer-toggle-pause)
  (define-key map (kbd "<right>") 'mplayer-seek-forward)
  (define-key map (kbd "<left>")  'mplayer-seek-backward)
  (define-key map (kbd "f")       'mplayer-faster)
  (define-key map (kbd "s")       'mplayer-slower)
  (define-key map (kbd "r")       'mplayer-reset-speed)
  (define-key map (kbd "p")       'mplayer-seek-position)
  (define-key map (kbd "t")       'mplayer-insert-position)
  (define-key map (kbd "d")       'mplayer-toggle-osd)
  (define-key map (kbd "i")       'mplayer-insert-timestamp)
  (define-key map (kbd "q")       'mplayer-quit-mplayer)

  (define-key mplayer-mode-map mplayer-prefix-command map))

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
