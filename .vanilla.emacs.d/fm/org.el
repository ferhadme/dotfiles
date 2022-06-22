;; Org mode configurations
(setq org-todo-keyword-faces
      '(
	("HACK" . (:foreground "blue" :weight normal))
	("PROGRESS" . (:foreground "purple" :weight normal))
	("IDEA" . (:foreground "yellow" :weight normal))
	("REFERENCE" . (:foreground "cyan" :weight normal))
	("URGENT" . (:foreground "#C03D29" :weight italic))
	("NOW" . (:foreground "#9E8EC0" :weight bold))
	("BUG" . (:foreground "#FF0000" :weight bold))
	))

(setq org-todo-keywords
      '((sequence "TODO" "URGENT" "BUG" "PROGRESS" "NOW" "REFERENCE" "IDEA" "HACK" "DONE")))

(setq org-hide-emphasis-markers t)
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c a") (kbd "C-u C-u C-u TAB"))))
(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c <up>") 'org-priority-up)))
(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c <down>") 'org-priority-down)))

(custom-theme-set-faces 'user
	`(org-level-1
	  ((t (:foreground "#97A7C8" :weight normal))))
	`(org-level-2
	  ((t (:foreground "#FFDD35" :weight normal))))
	`(org-level-3
	  ((t (:foreground "#F4F4FF" :weight normal))))
	`(org-level-4
	  ((t (:foreground "#CC8C3C" :weight normal))))
	`(org-level-5
	  ((t (:foreground "#CA26D9" :weight normal))))
	`(org-priority
	  ((t (:foreground "#b9fbc0" :weight normal))))
	`(org-block
	  ((t (:background "#262922")))))

(setq org-startup-with-inline-images t)

(setq org-src-fontify-natively t)

(set-face-attribute 'italic nil
                :slant 'oblique
                :underline nil)
