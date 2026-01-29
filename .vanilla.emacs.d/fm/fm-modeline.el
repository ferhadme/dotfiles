;;; fm-modeline.el --- mode-line customization  -*- lexical-binding: t; -*-


(defun fm/format-file-encoding ()
  (format "%s"
	  (upcase (symbol-name (coding-system-type buffer-file-coding-system)))))


(setq mode-line-modes nil)

(defun fm/mode-line-major-mode ()
  (propertize
   (format " %s " mode-name)
   'face '(:background "#3a3a3a"
		       :foreground "#ffcc66"
		       :weight bold)))


(setq-default mode-line-format
              '("%e"
		mode-line-front-space

		mode-line-modified
                (:eval (propertize (format " %s " (buffer-name))
                                   'face '(:background "#282828"
						       :foreground "#ffdd33"
						       :weight bold)))
		mode-line-modified
                "   "

		"L%l:C%c "

		(:eval (propertize " " 'display
				   `((space
				      :align-to
				      (- right
					 ,(+ 3
					     (length (format-mode-line '((vc-mode vc-mode))))
					     (length (format-mode-line 'mode-line-modes))
					     (length (format-mode-line 'mode-line-misc-info))
					     (length (fm/format-file-encoding))))))))

		(:eval (when vc-mode
			 (propertize (format " ⎇ %s " (substring vc-mode 5))
				     'face '(:foreground "#95a99f" :weight bold))))

                "  "

                (:eval (fm/mode-line-major-mode))
                mode-line-misc-info

		(:eval (fm/format-file-encoding))

                mode-line-end-spaces))

(set-face-attribute 'mode-line nil
                    :background "#181818"
                    :foreground "#e4e4ef"
                    :box nil)


(provide 'fm-modeline.el)
