;;; motion.el --- Various useful motions and editing commands -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 30 April 2023
;; Homepage: https://github.com/leotaku/motion
;; Keywords: emulations
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

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

;; TODO

;;; Code:

(defcustom motion-mode-function #'ignore
  "Mode function used for motions that change the editing mode.")

(defun motion-insert ()
  (interactive)
  (funcall motion-mode-function -1))

(defun motion-append ()
  (interactive)
  (when (/= (point) (point-at-eol))
    (forward-char))
  (motion-insert))

(defun motion-Insert ()
  (interactive)
  (back-to-indentation)
  (motion-insert))

(defun motion-Append ()
  (interactive)
  (end-of-line)
  (motion-insert))

(defun motion-change (arg)
  (interactive "p")
  (kill-region-or-line arg)
  (motion-insert))

(defun motion-forward-word (arg)
  (interactive "p")
  (motion-syntax arg nil "[:word:]" "^[:word:]"))

(defun motion-forward-end (arg)
  (interactive "p")
  (motion-syntax arg t "^[:word:]" "[:word:]"))

(defun motion-backward-word (arg)
  (interactive "p")
  (motion-forward-end (- arg)))

(defun motion-forward-Word (arg)
  (interactive "p")
  (motion-syntax arg nil "^[:space:]\n" "[:space:]\n"))

(defun motion-forward-End (arg)
  (interactive "p")
  (motion-syntax arg t "[:space:]\n" "^[:space:]\n"))

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
        (when (and (not (region-active-p)) (< 0 n)) (backward-char))
      (when (> 0 n) (backward-char)))))

(defvar-local motion-last-char nil)
(defvar-local motion-last-count nil)
(defvar-local motion-last-until nil)

(defun motion-goto-char (arg)
  (interactive "p")
  (let ((char (char-to-string (read-char))))
    (goto-char (motion-find-char char arg))
    (setq motion-last-char char)
    (setq motion-last-count arg)
    (setq motion-last-until nil)))

(defun motion-till-char (arg)
  (interactive "p")
  (let ((char (char-to-string (read-char))))
    (goto-char (motion-find-char char arg t))
    (setq motion-last-char char)
    (setq motion-last-count arg)
    (setq motion-last-until t)))

(defun motion-repeat-char (arg)
  (interactive "p")
  (when (null motion-last-char)
    (error "No previous jump that can be repeated"))
  (let* ((sign (/ motion-last-count (abs motion-last-count)))
         (point (motion-find-char motion-last-char sign motion-last-until)))
    (goto-char point)))

(defun motion-find-char (char count &optional until)
  (save-excursion
    (forward-char (if (< 0 count) (if until 2 1) (if until -2 -1)))
    (search-forward char nil nil count)
    (backward-char (if (< 0 count) (if until 2 1) (if until -1 0)))
    (point)))

(provide 'motion)

;;; motion.el ends here
