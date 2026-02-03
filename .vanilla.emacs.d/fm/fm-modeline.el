;;; fm-modeline.el --- mode-line customization  -*- lexical-binding: t; -*-

;; All hex codes are taken from gruber darker theme color palette
;; https://github.com/rexim/gruber-darker-theme/blob/master/gruber-darker-theme.el#LL59

;; Face definitions
;;
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

;; Mode-line modes (minor modes) faces
(defface fm/modes-face
  '((t :foreground "#95a99f"
       :weight bold))
  "Face for displaying minor modes in active modeline")


;; Formatters
;;
;; Buffer name
(defun fm/buffer-name ()
  (propertize
    (format " %s " (buffer-name))
    'face (if (mode-line-window-selected-p)
            'fm/buffer-name-face
            'fm/buffer-name-inactive-face)))

;; File encoding
(defun fm/format-file-encoding ()
  (format "%s" buffer-file-coding-system))

;; Alignment between left and right part of modeline
(defun fm/alignment ()
  (propertize " " 'display
	`((space
		:align-to
		(- right
		  ,(+ 2 ;; sum of manually used spaces in right side

			 (length (fm/mode-line-major-mode))
			 (length (format-mode-line mode-line-modes))
			 (length (fm/vc))))))))

(defun fm/ensure-space (formatted-str original-str)
  (concat formatted-str (if (= (length original-str) 0) "" " ")))

;; Major mode
(defun fm/mode-line-major-mode ()
  (propertize
    (format " %s " major-mode)
    'face (if (mode-line-window-selected-p)
            'fm/major-mode-face
            'fm/major-mode-inactive-face)))

;; Version control
(defun fm/vc ()
  (when vc-mode
    (let ((branch (substring vc-mode 5)))
      (propertize (fm/ensure-space (format "⎇ %s" branch) branch)
	    'face (if (mode-line-window-selected-p)
                'fm/vc-face
                'fm/vc-inactive-face)))))


;; Mode line format
(setq-default mode-line-format
  '("%e"
	 mode-line-front-space
	 mode-line-mule-info
     (:eval (fm/buffer-name))
	 mode-line-modified
     "  "
	 "L%l:C%c "
     "  "

	 (:eval (fm/alignment))

     (:propertize mode-line-modes face fm/modes-face)

     " "

	 (:eval (fm/mode-line-major-mode))
     " "

     (:eval (fm/vc))

     mode-line-end-spaces))

(provide 'fm-modeline)
