(defun rc/jump-to-newline ()
  "Jumps to newline from any position"
  (interactive)
  (end-of-line)
  (newline))

(defun rc/jump-to-newline-prog-mode ()
  "Jumps to newline from any position with indentation"
  (interactive)
  (rc/jump-to-newline)
  (indent-for-tab-command))

(defun rc/copy-line ()
  "Copies a line under the cursor"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank))

(defun rc/duplicate-line ()
  "Duplicates a line under the cursor"
  (interactive)
  (rc/copy-line)
  (rc/jump-to-newline)
  (yank))

(defun rc/enlarge-window ()
  "Increases the size of window by 15. Useful in pop-up windows like compilation"
  (interactive)
  (let ((size 15))
    (enlarge-window size)))
