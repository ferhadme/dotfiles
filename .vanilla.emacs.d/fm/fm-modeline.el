;;; fm-modeline.el --- mode-line customization  -*- lexical-binding: t; -*-

;; All hex codes are taken from gruber darker theme color palette
;; https://github.com/rexim/gruber-darker-theme/blob/master/gruber-darker-theme.el#LL59

;; Buffer name faces
(defface fm/buffer-name-face
  '((t :background "#282828"
      :foreground "#ffdd33"
      :weight bold))
  "Face for displaying buffer name in active modeline")

(defface fm/buffer-name-inactive-face
  '((t :background "#1c1c1c"
      :foreground "#666666"
      :weight normal))
  "Face for displaying buffer name in inactive modeline")

;; Major mode faces
(defface fm/major-mode-face
  '((t :background "#3a3a3a"
      :foreground "#ffcc66"
      :weight bold))
  "Face for displaying major mode in active modeline")

(defface fm/major-mode-inactive-face
  '((t :background "#252525"
      :foreground "#555555"
      :weight normal))
  "Face for displaying major mode in inactive modeline")

;; Version control faces
(defface fm/vc-face
  '((t :foreground "#95a99f"))
  "Face for displaying version control info in active modeline")

(defface fm/vc-inactive-face
  '((t :foreground "#444444"))
  "Face for displaying version control info in inactive modeline")

;; Buffer name
(defun fm/buffer-name ()
  (propertize
    (format " %s " (buffer-name))
    'face (if (mode-line-window-selected-p)
            'fm/buffer-name-face
            'fm/buffer-name-inactive-face)))

;; Major mode
(defun fm/mode-line-major-mode ()
  (propertize
    (format " %s " major-mode)
    'face (if (mode-line-window-selected-p)
            'fm/major-mode-face
            'fm/major-mode-inactive-face)))

;; File encoding
(defun fm/format-file-encoding ()
  (format "%s" buffer-file-coding-system))

;; Alignment between left and right part of modeline
(defun fm/alignment ()
  (propertize " " 'display
	`((space
		:align-to
		(- right
		  ,(+ 1 ;; Sum of manually used spaces after usage of fm/alignment in mode-line-format
			 (length (fm/mode-line-major-mode))
			 (length (fm/vc))))))))


;; Version control
(defun fm/vc ()
  (when vc-mode
    (propertize (format " ⎇ %s " (substring vc-mode 5))
	  'face (if (mode-line-window-selected-p)
              'fm/vc-face
              'fm/vc-inactive-face))))


;; Mode line format
(setq-default mode-line-format
  '("%e"
	 mode-line-front-space

	 mode-line-modified
     (:eval (fm/buffer-name))
	 mode-line-modified
     "  "

	 "L%l:C%c "

	 (:eval (fm/alignment))

	 (:eval (fm/mode-line-major-mode))

	 " "

	 (:eval (fm/vc))

     mode-line-end-spaces))

(provide 'fm-modeline)
