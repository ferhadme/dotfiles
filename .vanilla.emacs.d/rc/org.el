;; Org mode configurations
(setq org-todo-keyword-faces
      '(
        ("HACK" . (:foreground "blue" :weight normal))
	("PROGRESS" . (:foreground "purple" :weight normal))
	("IDEA" . (:foreground "yellow" :weight normal))
	("REFERENCE" . (:foreground "cyan" :weight normal))
	("URGENT" . (:foreground "#C03D29" :weight bold))
        ))

(setq org-todo-keywords
      '((sequence "TODO" "URGENT" "PROGRESS" "REFERENCE" "IDEA" "HACK" "DONE")))

(add-hook 'org-mode-hook
 	  (lambda () (local-set-key (kbd "C-c a") (kbd "C-u C-u C-u TAB"))))

(custom-theme-set-faces 'user
                        `(org-level-1 ((t (:foreground "#96A6C8" :weight normal))))
			`(org-level-2 ((t (:foreground "#CC8C3C" :weight normal))))
			`(org-level-3 ((t (:foreground "#F4F4FF" :weight normal))))
			`(org-level-4 ((t (:foreground "#F4F4FF" :weight normal))))
			`(org-level-5 ((t (:foreground "#F4F4FF" :weight normal)))))
