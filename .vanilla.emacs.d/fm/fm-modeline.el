;;; fm-modeline.el --- mode-line customization  -*- lexical-binding: t; -*-

;; Buffer name
(defun fm/buffer-name ()
  (propertize
   (format " %s " (buffer-name))
   'face '(:background "#282828"
		       :foreground "#ffdd33"
		       :weight bold)))

;; Major mode
(defun fm/mode-line-major-mode ()
  (propertize
   (format " %s " major-mode)
   'face '(:background "#3a3a3a"
		       :foreground "#ffcc66"
		       :weight bold)))

;; File encoding
(defun fm/format-file-encoding ()
  (format "%s"
	  (upcase (symbol-name (coding-system-type buffer-file-coding-system)))))

;; Alignment between left and right part of modeline
(defun fm/alignment ()
  (propertize " " 'display
	      `((space
		 :align-to
		 (- right
		    ,(+ 4 ;; Sum of spaces between elements
			(length (fm/mode-line-major-mode))
			(length (format-mode-line '((vc-mode vc-mode))))
			(length (format-mode-line 'mode-line-misc-info))
			(length (fm/format-file-encoding))))))))

;; Version control
(defun fm/vc ()
  (when vc-mode
    (propertize (format " ⎇ %s " (substring vc-mode 5))
		'face '(:foreground "#95a99f" :weight bold))))

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

		"  "

		(:eval (fm/vc))

                "  "

                mode-line-misc-info

		(:eval (fm/format-file-encoding))

                mode-line-end-spaces))

(provide 'fm-modeline.el)
