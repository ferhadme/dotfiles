(defun fm/jump-to-newline ()
  "Jumps to newline from any position"
  (interactive)
  (end-of-line)
  (newline))

(defun fm/jump-to-newline-prog-mode ()
  "Jumps to newline from any position with indentation"
  (interactive)
  (fm/jump-to-newline)
  (indent-for-tab-command))

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
  (fm/jump-to-newline)
  (yank))

(defun fm/enlarge-window ()
  "Increases the size of window by 15. Useful in pop-up windows like compilation"
  (interactive)
  (let ((size 15))
    (enlarge-window size)))
