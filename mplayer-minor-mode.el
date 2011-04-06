;;; mplayer-minor-mod.el - GNU Emacs mode to control mplayer, mainly
;;; to facilitate transcription and note-taking.

;; Copyright (C) 2007 Free Software Foundation, Inc.

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
;; Owes a lot, in idea and execution, to the emacs video editor gneve
;; (http://www.1010.co.uk/gneve.html).  This mode controls mplayer
;; directly, using its slave-mode (see
;; http://www.mplayerhq.hu/DOCS/tech/slave.txt), which allows commands
;; on stdin.

;;; Use: 

;;; Install:

;; Put something similar to the following in your ~/.emacs to use this file:
;;
;; (load "~/path/to/mplayer-minor-mode.el")
;;

;;; Dependency:

;; mplayer

;;; TODO:
;; - Proper org-mode integration would probably be nice (eg, a link to the file)
;; - Error handling and clean-up

;;; Code:

(defvar mplayer-executable "mplayer"
  "Name or path to the mplayer executable")

;; (defvar mplayer-process nil
;;   "The process controlling mplayer")

(defvar mplayer-mode-map nil
  "Local keymap for mplayer-minor-mode")

(defvar mplayer-prefix-command (kbd "C-x SPC")
  "The prefix for all mplayer minor-mode commands")

(defvar mplayer-default-seek-step 10
  "The number of seconds that the skip command will use.")

(defvar mplayer-osd-level 3
  "OSD level used by mplayer.  3 (the default) means position/length.")

;; (defvar mplayer-process-buffer nil
;;   "Buffer used for communication with the mplayer process.")

(defvar mplayer-timestamp-format "%H:%M:%S"
  "Format used for inserting timestamps.")


;;; Utilities:

(defun mplayer--send (cmd)
  (process-send-string mplayer-process (concat cmd "\n")))

(defun mplayer--parse-seconds (seconds)
  (cond
   ((null seconds) mplayer-default-seek-step)
   ((numberp seconds) seconds)
   ((listp seconds)
    (* mplayer-default-seek-step (log (abs (car seconds)) 4)))))

(defun mplayer--format-time (time)
  (message "format-time: %s" time)
  (if (stringp time)
      (setq time (round (string-to-number time))))
  (message "time to format: %s" time)
  (format-time-string mplayer-timestamp-format `(0 ,time 0) t))

;;; Interactive Commands:

(defun mplayer-find-file (filename)
  (interactive "fOpen movie file: ")
  (set (make-local-variable 'mplayer--osd-enabled) nil)
  (set (make-local-variable 'mplayer-process-buffer) (generate-new-buffer "*mplayer*"))
  (set (make-local-variable 'mplayer-process)
       (start-process "mplayer" mplayer-process-buffer
                      mplayer-executable
                      "-quiet" "-slave"
                      filename)))

(defun mplayer-toggle-pause ()
  (interactive)
  (mplayer--send "pause"))

(defun mplayer-seek-forward (seconds)
  (interactive "P")
  (let ((seconds (mplayer--parse-seconds seconds)))
    (mplayer--send (format "seek %d 0" seconds))))

(defun mplayer-seek-backward (seconds)
  (interactive "P")
  (let ((seconds (- (mplayer--parse-seconds seconds))))
    (mplayer--send (format "seek %d 0" seconds))))

(defun mplayer-toggle-osd ()
  (interactive)
  (if mplayer--osd-enabled
      (mplayer--send "osd")
    (mplayer--send (format "osd %d" mplayer-osd-level)))
  (setq mplayer--osd-enabled (not mplayer--osd-enabled)))

(defun mplayer-insert-timestamp ()
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

(defun mplayer-quit-mplayer ()
  (interactive)
  (mplayer--send "quit")
  (set-process-filter
   mplayer-process
   (lambda (process output)
     (kill-buffer mplayer-process-buffer))))

;;; Mode setup:

(unless mplayer-mode-map
  (setq mplayer-mode-map (make-sparse-keymap)))

(let ((map (make-sparse-keymap)))
  (define-key map (kbd "f")       'mplayer-find-file)
  (define-key map (kbd "SPC")     'mplayer-toggle-pause)
  (define-key map (kbd "<right>") 'mplayer-seek-forward)
  (define-key map (kbd "<left>")  'mplayer-seek-backward)
  (define-key map (kbd "d")       'mplayer-toggle-osd)
  (define-key map (kbd "t")       'mplayer-insert-timestamp)
  (define-key map (kbd "q")       'mplayer-quit-mplayer)

  (define-key mplayer-mode-map mplayer-prefix-command map))

(define-minor-mode mplayer-mode
  "Control mplayer from within Emacs.  Mainly intended for
transcription purposes, so commands exist to pause, seek, and
insert the current time as a timestamp:

Key bindings:
\\{mplayer-mode-map}"
  nil                                   ; initial value
  "MPlayer"                             ; mode-line string
  mplayer-mode-map)

(provide 'mplayer)