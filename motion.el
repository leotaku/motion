;;; motion.el --- Various useful text motions and editing commands -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 30 April 2023
;; Homepage: https://github.com/leotaku/motion
;; Keywords: emulations
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The motion package provides various text motions and editing
;; commands.  It can be used in conjunction with other packages and
;; built-in Emacs facilities to design your own modal editing setup.
;;
;; Please consult README.md from the package repository and elisp
;; docstrings for more thorough documentation.

(require 'seq)
(require 'keymap)

;;; Code:

(defgroup motion '()
  "Various useful text motions and editing commands."
  :group 'editing
  :prefix "modalka-")

(defcustom motion-mode-function #'ignore
  "Mode function used for motions that change the editing mode."
  :type 'function)

;;;###autoload
(defun motion-insert ()
  (interactive)
  (funcall motion-mode-function -1))

;;;###autoload
(defun motion-append ()
  (interactive)
  (when (/= (point) (line-end-position))
    (forward-char))
  (motion-insert))

;;;###autoload
(defun motion-Insert ()
  (interactive)
  (back-to-indentation)
  (motion-insert))

;;;###autoload
(defun motion-Append ()
  (interactive)
  (end-of-line)
  (motion-insert))

;;;###autoload
(defun motion-change (arg)
  (interactive "p")
  (motion-kill-region-or-line arg)
  (motion-insert))

;;;###autoload
(defun motion-forward-word (arg)
  (interactive "p")
  (motion-syntax arg nil "[:word:]" "^[:word:]"))

;;;###autoload
(defun motion-forward-end (arg)
  (interactive "p")
  (motion-syntax arg t "^[:word:]" "[:word:]"))

;;;###autoload
(defun motion-backward-word (arg)
  (interactive "p")
  (motion-forward-end (- arg)))

;;;###autoload
(defun motion-forward-Word (arg)
  (interactive "p")
  (motion-syntax arg nil "^[:space:]\n" "[:space:]\n"))

;;;###autoload
(defun motion-forward-End (arg)
  (interactive "p")
  (motion-syntax arg t "[:space:]\n" "^[:space:]\n"))

;;;###autoload
(defun motion-backward-Word (arg)
  (interactive "p")
  (motion-forward-End (- arg)))

(defun motion-syntax (n reverse-adjust &rest syntaxes)
  (let ((f (if (< 0 n) #'skip-chars-forward #'skip-chars-backward)))
    (if reverse-adjust
        (when (< 0 n) (forward-char))
      (when (> 0 n) (backward-char)))
    (dotimes (_ (abs n)) (mapc f syntaxes))
    (if reverse-adjust
        (when (and (not (use-region-p)) (< 0 n)) (backward-char))
      (when (> 0 n) (backward-char)))))

(defvar-local motion-last-char nil)
(defvar-local motion-last-count nil)
(defvar-local motion-last-until nil)

;;;###autoload
(defun motion-goto-char (arg)
  (interactive "p")
  (let ((char (char-to-string (read-char))))
    (goto-char (motion-find-char char arg))
    (setq motion-last-char char)
    (setq motion-last-count arg)
    (setq motion-last-until nil)))

;;;###autoload
(defun motion-till-char (arg)
  (interactive "p")
  (let ((char (char-to-string (read-char))))
    (goto-char (motion-find-char char arg t))
    (setq motion-last-char char)
    (setq motion-last-count arg)
    (setq motion-last-until t)))

;;;###autoload
(defun motion-repeat-char (arg)
  (interactive "p")
  (when (null motion-last-char)
    (error "No previous jump that can be repeated"))
  (let* ((sign (/ motion-last-count (abs motion-last-count)))
         (point (motion-find-char motion-last-char (* arg sign) motion-last-until)))
    (goto-char point)))

(defun motion-find-char (char count &optional until)
  (save-excursion
    (forward-char (if (< 0 count) (if until 2 1) (if until -2 -1)))
    (search-forward char nil nil count)
    (backward-char (if (< 0 count) (if until 2 1) (if until -1 0)))
    (point)))

;;;###autoload
(defun motion-replace-char (arg)
  (interactive "p")
  (let ((char (char-to-string (read-char))))
    (save-excursion
      (dotimes (_ arg) (delete-char 1) (insert char)))))

;;;###autoload
(defun motion-goto-or-quit (arg)
  (interactive "P")
  (if (numberp arg)
      (if (> arg 0)
          (funcall-interactively #'goto-line arg)
        (funcall-interactively #'goto-line (+ arg (line-number-at-pos (point-max)))))
    (funcall-interactively (keymap-lookup nil "C-g"))))

;;;###autoload
(defun motion-delete (arg)
  (interactive "p")
  (if (use-region-p)
      (mapc
       (lambda (it) (delete-region (car it) (cdr it)))
       (region-bounds))
    (delete-char arg)))

;;;###autoload
(defun motion-kill-region-or-line (arg)
  (interactive "p")
  (if (use-region-p)
      (call-interactively #'kill-region)
    (kill-whole-line arg)))

;;;###autoload
(defun motion-copy-region-or-line (arg)
  (interactive "p")
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let ((begin (line-beginning-position))
          (adapt (if (= arg 0) 0 (/ arg (abs arg)))))
      (save-excursion
        (condition-case nil (forward-line (- arg adapt)) (quit))
        (copy-region-as-kill begin (line-end-position))
        (kill-append "\n" nil)))))

;;;###autoload
(defun motion-put-before (arg)
  (interactive "p")
  (let ((kill (current-kill 0)))
    (if (string-suffix-p "\n" kill)
        (save-excursion
          (goto-char (line-beginning-position))
          (dotimes (_ arg) (insert kill)))
      (dotimes (_ arg) (insert kill)))))

;;;###autoload
(defun motion-put-after (arg)
  (interactive "p")
  (let ((kill (current-kill 0)))
    (if (string-suffix-p "\n" kill)
        (save-excursion
          (when (= (line-end-position) (point-max))
            (goto-char (point-max))
            (newline))
          (forward-line 1)
          (dotimes (_ arg) (insert kill))
          (forward-line -2))
      (dotimes (_ arg)
        (insert kill)
        (backward-char (length kill))))))

;;;###autoload
(defun motion-kill-uncycle (arg)
  (interactive "p")
  (motion-kill-cycle (- arg)))

;;;###autoload
(defun motion-kill-cycle (arg)
  (interactive "p")
  (let* ((length (seq-length kill-ring))
         (to-append (% (+ length arg) length))
         (_ (current-kill 0)))
    (setq kill-ring (append (seq-drop kill-ring to-append)
                            (seq-take kill-ring to-append)))))

;;;###autoload
(defun motion-mark-cycle ()
  (interactive)
  (if (use-region-p)
      (if (null rectangle-mark-mode)
          (rectangle-mark-mode)
        (deactivate-mark))
    (set-mark-command nil)))

(provide 'motion)

;;; motion.el ends here
