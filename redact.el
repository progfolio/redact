;;; redact.el --- Visually redact buffer text. -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Redact.el provides functions for visually redacting text
;; as well as a `redact-mode' to dynamically redact portions of text.
;; Redact works in conjunction with `reveal-mode', which see.

;;; Code:
(defgroup redact nil "Redact buffer text." :group 'applications :prefix "redact-")
;;@MAYBE: some of these user options should have setters which recompute redactions
(defcustom redact-replacement "*" "Replacement used by `redact-string'."
  :type '(choice string function))
(defcustom redaction-function #'redact-string
  "Unary function which must take a string and return a redacted string."
  :type 'function)
(defcustom redact-mode-function #'redact-window-paragraphs
  "Function run during `post-command-hook' when `redact-mode' enabled."
  :type 'function)
(defcustom redact-mode-ignored-commands '(self-insert-command)
  "List of command symbols to ignore when `redact-mode' enabled."
  :type '(list symbol))
(defcustom redact-ignore-functions nil
  "Abnormal hook run during `post-command-hook'.
If any of the functions return non-nil, `redact-mode-function' is not called."
  :type 'function)
(defcustom redact-string-regexp "[^[:space:]\\|\n]" "Regexp used in `redact-string'."
  :type 'regexp)

(defun redact-string (string)
  "Replace STRING's chars with `redact-replacement'."
  (if (characterp redact-replacement)
      (make-string (length string) redact-replacement)
    (replace-regexp-in-string redact-string-regexp redact-replacement string)))

(defun redact--overlay (beg end &rest props)
  "Return overlay from BEG to END with PROPS."
  (let ((o (make-overlay beg end)))
    (while props (overlay-put o (pop props) (pop props)))
    o))

(defun redact--toggle (overlay hidep)
  "Toggle OVERLAY redaction according to HIDEP."
  (if (not hidep)
      (overlay-put overlay 'display nil)
    (if-let ((cache (overlay-get overlay 'redact-cache)))
        (overlay-put overlay 'display cache)
      (redact-region (overlay-start overlay) (overlay-end overlay)))))

;;;###autoload
(defun redact-region (beg end)
  "Redact BEG to END via `redaction-function'."
  (interactive (list (point) (mark)))
  (when redaction-function
    (let ((cache (funcall redaction-function (buffer-substring beg end))))
      (redact--overlay beg end 'redacted t 'display cache 'redact-cache cache
                       'reveal-toggle-invisible #'redact--toggle))))

;;;###autoload
(defun redact-unredact-region (beg end)
  "Remove redaction overlays from region BEG to END."
  (interactive "r")
  (mapc (lambda (o) (when (overlay-get o 'redacted) (delete-overlay o)))
        (overlays-in beg end)))

(defmacro redact-window (&rest body)
  "Eval BODY with point at `window-start'.
`window-start' and `window-end' are bound to `start` and `end`."
  (declare (debug t))
  `(save-excursion (let ((start (window-start))
                         (end (window-end)))
                     (redact-unredact-region start end)
                     (goto-char (window-start))
                     ,@body)))

(defun redact-window-lines ()
  "Redact each window line."
  (redact-window (while (and (< (point) end) (not (eobp)))
                   (redact-region (line-beginning-position) (line-end-position))
                   (forward-line 1))))

(defun redact-window-paragraphs ()
  "Redact each paragraph in window."
  (redact-window (while (and (< (point) end) (not (eobp)))
                   (redact-region (point) (progn (forward-paragraph) (point))))))

(defun redact--maybe () "Call `redact-mode-function'.
`redact-mode-ignore-functions' and `redact-mode-ignored-commands' are respected."
       (unless (or (memq this-command redact-mode-ignored-commands)
                   (run-hook-with-args-until-success 'redact-ignore-functions))
         (funcall redact-mode-function)))

;;;###autoload
(define-minor-mode redact-mode
  "Auto redact buffer text."
  :lighter " redact"
  (if redact-mode (add-hook 'post-command-hook #'redact--maybe -90 t)
    (remove-hook 'post-command-hook #'redact--maybe t)
    (redact-unredact-region (point-min) (point-max))))

(provide 'redact)
;;; redact.el ends here
