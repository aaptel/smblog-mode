;;; smblog.el --- samba log viewer

;; Copyright (C) 2016 Aurélien Aptel <aaptel@suse.com>

;; Author: Aurélien Aptel <aaptel@suse.com>
;; URL: http://github.com/aaptel/smblog-mode
;; Version: 1.0

;;; Commentary:

;; Emacs major mode for samba-formated logs.
;; - hilighting of log meta data messages
;; - easy navigation (`n` and `p`)
;; - filter by log level (`+` and `-`), files&functions (`f`)
;; - go to the source file (`RET` on any part of the log message)

;;; License:

;; MIT License
;;
;; Copyright (c) 2016 Aurélien Aptel <aaptel@suse.com>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'cl)
(require 'rx)

;;;###autoload
(add-to-list
 'auto-mode-alist
 (cons (rx (or "/" bos) "log." (or "nmbd" "smbd" "winbindd" "wb-"))
       'smblog-mode-from-file-buffer))

;;;###autoload
(defcustom smblog-src-dir (expand-file-name "~/prog/samba-git")
  "Path to samba source")

(defvar smblog-time-rx (rx
			"["
			(+ (or num "/")) ;; day
			(+ " ")
			(+ (or num ":" ".")) ;; time
			"," (+ " ")
			(group (+ num)) ;; debug level (1)
			(opt
			 "," (+ " ")
			 "pid=" (+ num)
			 "," (+ " ")
			 (+ (not (any "]")))) ;; effective/real uid/gid
			"]"
			" "
			(group (+ graphic)) ;; file (2)
			":"
			(group (+ num)) ;; line (3)
			"("
			(group (+ (not (any ")")))) ;; function (4)
			")"
			"\n")
  "Regex matching a log message header")


(defvar-local smblog-log-file nil "Current file being viewed in the buffer")
(defvar-local smblog-log-data nil "Vector of parsed log file")
(defvar-local smblog-pos-map nil "Vector mapping id to point position")
(defvar-local smblog-filter-level 10 "Current log level being displayed")
(defvar-local smblog-filter-file nil "Current log file filter")
(defvar-local smblog-filter-fun nil "Current log function filter")

(defun smblog-buf-name (file)
  "Return buffer name to be used to view FILE."
  (format "*smblog: %s*" file))

;;;###autoload
(defun smblog-mode-from-file-buffer ()
  "Create a viewer buffer from current buffer.
The buffer must be visiting an actual file."
  (interactive)
  (smblog-open (buffer-file-name)))

;;;###autoload
(defun smblog-open (file)
  "Create a viewer buffer of FILE."
  (interactive "flog file: ")
  (let ((buf-name (smblog-buf-name file)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((buffer-read-only nil))
	(erase-buffer))
      (smblog-mode)
      (let ((buffer-read-only nil))
	(setq smblog-log-file file)
	(setq smblog-log-data (smblog-parse file))
	(smblog-insert-log)
	(goto-char (point-min))))
    (switch-to-buffer buf-name)))

(defun smblog-parse (file)
  "Parse FILE and return a vector of log messages."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (msgs)
      (while (search-forward-regexp smblog-time-rx nil 'noerror)
	(let ((level (string-to-int (match-string 1)))
	      (file (match-string 2))
	      (nb (string-to-int (match-string 3)))
	      (fun (match-string 4))
	      (start (point))
	      txt)

	  (while (and (not (eobp)) (not (looking-at (rx bol "[20"))))
	    (forward-line))
	  (setq txt (buffer-substring start (point)))
	  (push (list level file nb fun txt) msgs)))
      (apply 'vector (nreverse msgs)))))

(defun smblog-insert-log (&optional filt-level filt-file filt-fun)
  "Insert filtered/processed log content."
  (setq filt-level (or filt-level 10))
  (let ((len (length smblog-log-data))
	(filt-file-rx (and filt-file (smblog-glob-to-rx filt-file)))
	(filt-fun-rx (and filt-fun (smblog-glob-to-rx filt-fun))))

    (setq smblog-pos-map (make-vector len nil))

    (dotimes (i len)
      (let* ((msg (aref smblog-log-data i))
	     (level (car msg))
	     (file (cadr msg))
	     (nb (caddr msg))
	     (fun (cadddr msg))
	     (txt (car (cddddr msg))))
	(when (and (<= level filt-level)
		   (or (null filt-file-rx) (string-match filt-file-rx file))
		   (or (null filt-fun-rx) (string-match  filt-fun-rx fun)))
	  (aset smblog-pos-map i (point))
	  (insert
	   (propertize
	    (concat
	     (propertize (format "[%2d " level) 'face '(:family "DejaVu Sans" :foreground "#999999"))
	     (propertize file 'face '(:foreground "#119911"))
	     (propertize (format ":%d " nb) 'face '(:family "DejaVu Sans" :foreground "#999999"))
	     (propertize fun 'face '(:foreground "#5555ff"))
	     (propertize "]" 'face '(:family "DejaVu Sans" :foreground "#999999"))
	     "\n" txt)
	    'smblog-index i)))))))

(defun smblog-next-msg ()
  "Move point to the beginning of the next message."
  (interactive)
  (let ((pos (next-single-property-change (point) 'smblog-index)))
    (when pos
      (goto-char pos)
      (forward-line))))

(defun smblog-prev-msg ()
  "Move point to the beginning of the previous message."
  (interactive)
  (let ((id (smblog-current-id)))
    (when (< 0 id)
      (smblog-move-close-to-id (1- id) -1)
      (forward-line))))

(defun smblog-current-id ()
  "Return the index in the parsed log vector of the message under the point."
  (save-excursion
    (when (= (point) (point-max))
      (backward-char))
    (get-text-property (point) 'smblog-index)))

(defun smblog-current-msg ()
  "Return the parsed message under the point."
  (aref smblog-log-data (or (smblog-current-id) (error "No log message under point"))))

(defun smblog-full-path (raw-file)
  "Return the full path of RAW-FILE."
  (let ((file (if (string-match
		   (rx bos (? "../") (group (+ any)) eos) raw-file)
		  (match-string 1 file)
		(error "Invalid file %s" raw-file)))
	(dir (progn (string-match (rx bos (group (+ any)) (? "/") eos) smblog-src-dir)
		    (match-string 1 smblog-src-dir))))
    (concat dir "/" file)))

(defun smblog-goto-src ()
  "Open the file that emited the message under the point."
  (interactive)
  (let* ((msg (smblog-current-msg))
	 (file (nth 1 msg))
	 (ln (nth 2 msg))
	 (fullpath (smblog-full-path file)))
    (if (null (file-exists-p fullpath))
	(message (concat "Cannot open file %s\n"
			 "smblog-src-dir: %s\n"
			 "          file: %s (any leading ../ removed)\n\n"
			 "User M-x set-variable smblog-src-dir RET to make it point to the right directory")
		 fullpath smblog-src-dir file)
      (find-file-other-window fullpath)
      (goto-line ln))))

(defun smblog-inc-level ()
  "Increase verbosity of current log by 1 level."
  (interactive)
  (if (>= smblog-filter-level 10)
      (progn
	(setq smblog-filter-level 10)
	(message "Already at log level 10."))
    (incf smblog-filter-level)
    (smblog-update)
    (message "Showing log up to level %d" smblog-filter-level)))

(defun smblog-dec-level ()
  "Decrease verbosity of current log by 1 level."
  (interactive)
  (if (<= smblog-filter-level 0)
      (progn
	(setq smblog-filter-level 0)
	(message "Already at log level 0."))
    (decf smblog-filter-level)
    (smblog-update)
    (message "Showing log up to level %d" smblog-filter-level)))

(defun smblog-move-close-to-id (id step)
  "Move point to log ID if visible, otherwise move to closest visible log it in the direction of STEP."
  (let ((p (aref smblog-pos-map id)))
    (if p
	(goto-char p)
      (while (and (>= id 0) (< id (length smblog-pos-map)) (null (aref smblog-pos-map id)))
	(incf id step))
      (when (>= id 0)
	(goto-char (aref smblog-pos-map id))))))

(defun smblog-update ()
  "Regenerate buffer content based on current filters."
  (let* ((buffer-read-only nil)
	 (id (smblog-current-id))
	 (offset (- (point) (aref smblog-pos-map id))))
    (erase-buffer)
    (smblog-insert-log smblog-filter-level smblog-filter-file smblog-filter-fun)
    (smblog-move-close-to-id id -1)
    (when (= id (smblog-current-id))
      (forward-char offset))))

(defun smblog-glob-to-rx (glob)
  (setq glob (replace-regexp-in-string (rx (+ "*")) "*" glob))
  (mapconcat 'regexp-quote (split-string glob (rx "*")) ".*"))

(defun smblog-filter-menu ()
  (interactive)
  (let ((c (read-char-choice "log filter:   f[i]le    f[u]nction   [r]eset   [q]uit ? " '(?i ?u ?r ?q))))
    (cond
     ((= c ?i)
      (setq smblog-filter-file (read-string "file pattern (use * as wildcard)? "))
      (smblog-update))
     ((= c ?u)
      (setq smblog-filter-fun (read-string "func pattern (use * as wildcard)? "))
      (smblog-update))
     ((= c ?r)
      (setq smblog-filter-file nil
	    smblog-filter-fun nil)
      (smblog-update))))
  (message "filter set to %s"
	   (mapconcat 'identity (list
				 (format "level <= %d" smblog-filter-level)
				 (if smblog-filter-file (format "file <%s>" smblog-filter-file) "all file")
				 (if smblog-filter-fun (format "func <%s>" smblog-filter-fun) "all func"))
		      " and ")))


;;;###autoload
(define-derived-mode smblog-mode special-mode "Smblog"
  "Major mode for viewing samba log files.
\\{smblog-mode-map}"
  (define-key smblog-mode-map (kbd "n")   'smblog-next-msg)
  (define-key smblog-mode-map (kbd "p")   'smblog-prev-msg)
  (define-key smblog-mode-map (kbd "s")   'smblog-goto-src)
  (define-key smblog-mode-map (kbd "RET") 'smblog-goto-src)
  (define-key smblog-mode-map (kbd "+")   'smblog-inc-level)
  (define-key smblog-mode-map (kbd "-")   'smblog-dec-level)
  (define-key smblog-mode-map (kbd "f")  'smblog-filter-menu))

(provide 'smblog)
;;; smblog.el ends here
