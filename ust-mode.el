;;; ust-mode.el --- Major mode for UTAU projects -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Homepage: https://kisaragi-hiu.com/projects/ust-mode
;; Keywords: languages


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Major mode for UTAU projects (UST), based on `conf-mode'.

;;; Code:

(require 'conf-mode)

;;;; Commands

(defun ust-normalize-paths ()
  "Normalize paths in current buffer.

Assuming the UST we're editing is named main.ust, this sets
OutFile to main.wav, CacheDir to main.cache, and if ProjectName
contains a dot, to main.ust."
  (interactive)
  (save-excursion
    (setf (point) (point-min))
    (while (re-search-forward
            (rx bol (or "OutFile" "CacheDir" "ProjectName") "="
                (group (zero-or-more any))
                "." (or "wav" "cache" "ust")
                eol)
            nil t)
      (replace-match (file-name-base buffer-file-name)
                     nil t nil 1))))

;;;; Major mode

(defconst ust-mode-syntax-table
  (let ((table conf-mode-syntax-table))
    ;; # and ; are both not comments
    (modify-syntax-entry ?# "_" table)
    (modify-syntax-entry ?\; "_" table)
    ;; quotes do not denote strings
    (modify-syntax-entry ?\' "_" table)
    (modify-syntax-entry ?\" "_" table)
    table)
  "Syntax table for `ust-mode'.")

(defconst ust-mode-font-lock-keywords
  `(("\\(#VERSION\\|#SETTING\\|#TRACKEND\\)" 1 'font-lock-keyword-face)
    ("\\(?:Label=\\|Lyric=\\)\\(.+\\)" 1 'font-lock-string-face)
    ("=\\(True\\|False\\)" 1 'font-lock-constant-face)
    ,@conf-font-lock-keywords)
  "Syntax highlighting keywords for `ust-mode'.")

;;;###autoload
(define-derived-mode ust-mode conf-mode "UTAU Sequence Text"
  "Major mode for editing UTAU project files."
  (set (make-local-variable 'font-lock-defaults)
       `((ust-mode-font-lock-keywords)
         nil t nil nil))
  (add-hook 'before-save-hook #'ust-normalize-paths nil t))

;;;; Provide

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ust\\'" . ust-mode))

(provide 'ust-mode)

;;; ust-mode.el ends here
