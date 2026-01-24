;;; fm-helpers.el --- Helper functions  -*- lexical-binding: t; -*-

(defvar fm/package-contents-refreshed nil
  "Flag to ensure package-refresh-contents runs only once per session.")

(defun fm/copy-line ()
  "Copies a line under the cursor"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank))

(defun fm/duplicate-line ()
  "Duplicates a line under the cursor"
  (interactive)
  (fm/copy-line)
  (newline)
  (yank))

(defun fm/enlarge-window ()
  "Increases the size of window by 15. Useful in pop-up windows like compilation"
  (interactive)
  (let ((size 15))
    (enlarge-window size)))

(defun fm/require (package)
  "Require wrapper to install required package if it is not installed"
  (unless (package-installed-p package)
    (when (not fm/package-contents-refreshed)
      (package-refresh-contents)
      (setq fm/package-contents-refreshed t))
    (package-install package))
  (require package))

(provide 'fm-helpers)

