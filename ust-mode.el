;;; ust-mode.el --- Major mode for UTAU projects -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.3.0
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
(require 'cl-lib)

;;;; Commands

(cl-defun ust-mode-normalize-paths (&optional (path (buffer-file-name)))
  "Normalize paths in current buffer to PATH.

Assuming the UST we're editing is named main.ust, this sets
OutFile to main.wav, CacheDir to main.cache, and ProjectName to
main.ust."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx bol (or "OutFile" "CacheDir" "ProjectName") "="
                (group (*? any))
                (opt "." (or "wav" "cache" "ust"))
                eol)
            nil t)
      (replace-match (file-name-base path) nil t nil 1))))

;;;###autoload
(defun ust-mode-normalize-paths-in-file (file)
  "Normalize paths in FILE.

Suitable for use in eshell:

mapc #\\='ust-mode-normalize-paths-in-file *.ust"
  (interactive "FNormalize paths in UST File: ")
  (with-temp-file file
    (insert-file-contents file)
    (ust-mode-normalize-paths file)))

(defun ust-mode-insert-notes-from-text (text)
  "Insert each character in TEXT as UTAU notes."
  (interactive "MText: ")
  (end-of-line)
  (mapc
   (lambda (char)
     (insert
      ;; UTAU will deal with the note ID itself
      (format "
[#0000]
Length=240
Lyric=%c
NoteNum=60
Intensity=100
Modulation=0
PreUtterance=")))
   text))

(defun ust-mode-skeleton ()
  "Insert a template such that this buffer is a valid UST when saved."
  (interactive)
  (insert
   "[#VERSION]
UST Version1.2
[#SETTING]
Tempo=150.00
Tracks=1
ProjectName=main
VoiceDir=%VOICE%闇音レンリ・連続音Ver1.5
OutFile=main.wav
CacheDir=main.cache
Tool1=wavtool.exe
Tool2=tool2/tn_fnds.exe
Mode2=True
[#0000]
Length=480
Lyric=R
NoteNum=60
PreUtterance=
[#TRACKEND]")
  (set-buffer-file-coding-system
   'japanese-shift-jis-dos)
  (save-buffer))

;;;; Major mode

(defconst ust-mode-syntax-table
  (let ((table (make-syntax-table conf-mode-syntax-table)))
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
  (setq-local font-lock-defaults
              `((ust-mode-font-lock-keywords)
                nil t nil nil))
  ;; (when (eql 0 (buffer-size))
  ;;   (ust-mode-skeleton))
  (add-hook 'before-save-hook #'ust-mode-normalize-paths nil t))

;;;; Provide

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ust\\'" . ust-mode))

(provide 'ust-mode)

;;; ust-mode.el ends here
