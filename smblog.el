;;; smblog.el --- samba log viewer

;; Copyright (C) 2016 Aurélien Aptel <aaptel@suse.com>

;; Author: Aurélien Aptel <aaptel@suse.com>
;; URL: http://github.com/aaptel/smblog-mode
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Emacs major mode for samba-formated logs.
;; - hilighting of log meta data messages
;; - easy navigation (`n` and `p`)
;; - filter by log level (`+` and `-`), files&functions (`f`)
;; - go to the source file (`RET` on any part of the log message)
;; - hilight regexes (ip addresses, pointers, users, ...) with different colors (`h`)
;; - expand and collapse messages with `TAB`

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

(require 'cl-lib)
(require 'rx)

;;;###autoload
(add-to-list
 'auto-mode-alist
 (cons (rx (or "/" bos) "log." (or "nmbd" "smbd" "winbindd" "wb-"))
       'smblog-mode-from-file-buffer))

;;;###autoload
(defcustom smblog-src-dir (expand-file-name "~/prog/samba-git")
  "Path to samba source")

(defconst smblog-time-rx (rx
			"["
			(group (+ (or num "/"))) ;; day (1)
			(+ " ")
			(group (+ (or num ":" "."))) ;; time (2)
			"," (+ " ")
			(group (+ num)) ;; debug level (3)
			(opt
			 "," (+ " ")
			 "pid=" (+ num)
			 "," (+ " ")
			 (+ (not (any "]")))) ;; effective/real uid/gid
			"]"
			" "
			(group (+ graphic)) ;; file (4)
			":"
			(group (+ num)) ;; line (5)
			"("
			(group (+ (not (any ")")))) ;; function (6)
			")"
			"\n")
  "Regex matching a log message header")


(defface smblog-file-face
  '((t . (:foreground "#119911")))
  "Face used for the file path in a log message metadata.")

(defface smblog-metadata-face
  '((t . (:foreground "#999999")))
  "Face used for a log message metadata.")

(defface smblog-fun-face
  '((t . (:foreground "#5555ff")))
  "Face used for the function name in a message metadata.")

(defface smblog-date-face
  '((t . (:foreground "#ff5555")))
  "Face used for date and time in a log message metadata.")

(defface smblog-hl-1-face
  '((t . (:weight bold :background "orange")))
  "Face #1 used for message hilights.")

(defface smblog-hl-2-face
  '((t . (:weight bold :background "green")))
  "Face #2 used for message hilights.")

(defface smblog-hl-3-face
  '((t . (:weight bold :background "blue")))
  "Face #3 used for message hilights.")

(defface smblog-hl-4-face
  '((t . (:weight bold :background "yellow")))
  "Face #3 used for message hilights.")


(defcustom smblog-hl-face-list '(smblog-hl-1-face
				 smblog-hl-2-face
				 smblog-hl-3-face
				 smblog-hl-4-face)
  "Faces used for highlighting in messages.")

(defvar-local smblog-log-file nil "Current file being viewed in the buffer")
(defvar-local smblog-log-data nil "Vector of parsed log file")
(defvar-local smblog-pos-map nil "Vector mapping id to point position")
(defvar-local smblog-visible-map nil "Bool-vector mapping id to visibility (`t' for visible)")
(defvar-local smblog-filter-level 10 "Current log level being displayed")
(defvar-local smblog-filter-file nil "Current log file filter")
(defvar-local smblog-filter-fun nil "Current log function filter")
(defvar-local smblog-hl-list nil "Current log highlight-regex list")

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
	(setq smblog-visible-map (make-bool-vector (length smblog-log-data) t))
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
	(let ((day (match-string 1))
	      (time (match-string 2))
	      (level (string-to-int (match-string 3)))
	      (file (match-string 4))
	      (nb (string-to-int (match-string 5)))
	      (fun (match-string 6))
	      (start (point))
	      txt)

	  (while (and (not (eobp)) (not (looking-at (rx bol "[20"))))
	    (forward-line))
	  (setq txt (buffer-substring start (point)))
	  (push (list day time level file nb fun txt) msgs)))
      (apply 'vector (nreverse msgs)))))

(defun smblog-hl-propertize (txt hls)
  (let ((faces smblog-hl-face-list))
    (dolist (hl hls)
      (let ((i 0)
	    (face (car faces)))
	(while (string-match hl txt i)
	  (setq
	   txt (replace-match (propertize (match-string 0 txt) 'face face) nil t txt)
	   i (match-end 0))))
      (setq faces (if (cdr faces) (cdr faces) faces))))
  txt)

(defun smblog-insert-log (&optional filt-level filt-file filt-fun hl-list)
  "Insert filtered/processed log content."
  (setq filt-level (or filt-level 10))
  (let ((len (length smblog-log-data))
	(filt-file-rx (and filt-file (smblog-glob-to-rx filt-file)))
	(filt-fun-rx (and filt-fun (smblog-glob-to-rx filt-fun))))

    (setq smblog-pos-map (make-vector len nil))

    (dotimes (i len)
      (let* ((msg (aref smblog-log-data i))
	     (day (nth 0 msg))
	     (time (nth 1 msg))
	     (level (nth 2 msg))
	     (file (nth 3 msg))
	     (nb  (nth 4 msg))
	     (fun (nth 5 msg))
	     (txt (nth 6 msg)))
	(when (and (<= level filt-level)
		   (or (null filt-file-rx) (string-match filt-file-rx file))
		   (or (null filt-fun-rx) (string-match  filt-fun-rx fun)))
	  (aset smblog-pos-map i (point))
	  (insert
	   (propertize
	    (concat
	     (propertize (format "[%2d " level) 'face 'smblog-metadata-face)
	     (propertize day 'face 'smblog-date-face)
	     (propertize time 'face 'smblog-date-face)
	     " "
	     (propertize file 'face 'smblog-file-face)
	     (propertize (format ":%d " nb) 'face 'smblog-metadata-face)
	     (propertize fun 'face 'smblog-fun-face)
	     (propertize "]" 'face 'smblog-metadata-face)
	     "\n" (propertize (if hl-list (smblog-hl-propertize txt hl-list) txt) 'invisible nil))
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
		  (match-string 1 raw-file)
		(error "Invalid file %s" raw-file)))
	(dir (progn (string-match (rx bos (group (+ any)) (? "/") eos) smblog-src-dir)
		    (match-string 1 smblog-src-dir))))
    (concat dir "/" file)))

(defun smblog-goto-src ()
  "Open the file that emited the message under the point."
  (interactive)
  (let* ((msg (smblog-current-msg))
	 (file (nth 3 msg))
	 (ln (nth 4 msg))
	 (fullpath (smblog-full-path file)))
    (if (null (file-exists-p fullpath))
	(message (concat "Cannot open file %s\n"
			 "smblog-src-dir: %s\n"
			 "          file: %s (any leading ../ removed)\n\n"
			 "User M-x set-variable smblog-src-dir RET \"your-samba/path\" RET "
			 "to make it point to the right directory")
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
    (cl-incf smblog-filter-level)
    (smblog-update)
    (message "Showing log up to level %d" smblog-filter-level)))

(defun smblog-dec-level ()
  "Decrease verbosity of current log by 1 level."
  (interactive)
  (if (<= smblog-filter-level 0)
      (progn
	(setq smblog-filter-level 0)
	(message "Already at log level 0."))
    (cl-decf smblog-filter-level)
    (smblog-update)
    (message "Showing log up to level %d" smblog-filter-level)))

(defun smblog-move-close-to-id (id step)
  "Move point to log ID if visible, otherwise move to closest visible log it in the direction of STEP."
  (let ((p (aref smblog-pos-map id)))
    (if p
	(goto-char p)
      (while (and (>= id 0) (< id (length smblog-pos-map)) (null (aref smblog-pos-map id)))
	(cl-incf id step))
      (when (>= id 0)
	(goto-char (aref smblog-pos-map id))))))

(defun smblog-update ()
  "Regenerate buffer content based on current filters."
  (let* ((buffer-read-only nil)
	 (id (smblog-current-id))
	 (offset (- (point) (aref smblog-pos-map id))))
    (erase-buffer)
    (smblog-insert-log smblog-filter-level smblog-filter-file smblog-filter-fun smblog-hl-list)

    ;; restore collapse state
    (dotimes (i (length smblog-log-data))
      (let ((p (aref smblog-pos-map i))
	    (v (not (aref smblog-visible-map i))))
	(when (and p v)
	  (goto-char p)
	  (forward-line)
	  (put-text-property (point)
			     (or
			      (next-single-property-change (point) 'smblog-index)
			      (point-max))
			     'invisible t))))

    (smblog-move-close-to-id id -1)
    (when (= id (smblog-current-id))
      (forward-char offset))))

(defun smblog-glob-to-rx (glob)
  (setq glob (replace-regexp-in-string (rx (+ "*")) "*" glob))
  (mapconcat 'regexp-quote (split-string glob (rx "*")) ".*"))

(defun smblog-hl-menu ()
  (interactive)
  (let ((c (read-char-choice "hilight:  [a]dd  [r]eset   [q]uit ? " '(?a ?r ?q))))
    (cond
     ((= c ?a)
      (add-to-list 'smblog-hl-list (read-regexp "regex? ") t)
      (smblog-update))
     ((= c ?r)
      (setq smblog-hl-list nil)
      (smblog-update)))))

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

(defun smblog-set-buffer-source-tree (dir)
  "Change current buffer C source directory."
  (interactive "Ddir? ")
  (set (make-local-variable 'smblog-src-dir) dir))

(defun smblog-toggle-msg ()
  "Collapse or expand current message under point."
  (interactive)
  (when (bolp)
    (end-of-line))

  (let* ((id (smblog-current-id))
	 (beg-msg (aref smblog-pos-map id))
	 (end (or (next-single-property-change beg-msg 'smblog-index) (point-max)))
	 (beg-txt (save-excursion (goto-char beg-msg) (forward-line) (point))))
    ;;(message "<%s>" (buffer-substring-no-properties beg end))
    (goto-char beg-txt)
    (let ((buffer-read-only nil)
	  (new-val (not (get-text-property (point) 'invisible))))
      (put-text-property beg-txt end 'invisible new-val)
      (aset smblog-visible-map id (not new-val)))
    (goto-char beg-msg)))

(defun smblog-expand-all ()
  "Expand all log in the current buffer."
  (interactive)
  (setq smblog-visible-map (make-bool-vector (length smblog-visible-map) t))
  (let ((buffer-read-only nil))
    (remove-list-of-text-properties (point-min) (point-max) '(invisible))))

;;;###autoload
(define-derived-mode smblog-mode special-mode "Smblog"
  "Major mode for viewing samba log files.
\\{smblog-mode-map}"
  ;;(add-to-invisibility-spec '(t . t))
  (define-key smblog-mode-map (kbd "n")   'smblog-next-msg)
  (define-key smblog-mode-map (kbd "p")   'smblog-prev-msg)
  (define-key smblog-mode-map (kbd "s")   'smblog-goto-src)
  (define-key smblog-mode-map (kbd "RET") 'smblog-goto-src)
  (define-key smblog-mode-map (kbd "+")   'smblog-inc-level)
  (define-key smblog-mode-map (kbd "-")   'smblog-dec-level)
  (define-key smblog-mode-map (kbd "f")   'smblog-filter-menu)
  (define-key smblog-mode-map (kbd "h")   'smblog-hl-menu)
  (define-key smblog-mode-map (kbd "TAB") 'smblog-toggle-msg))

(provide 'smblog)
;;; smblog.el ends here
