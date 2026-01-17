;;; fm-org.el --- Org configuration  -*- lexical-binding: t; -*-

;; Org mode configurations
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "CANCEL(c)")))

(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "#ef6c00" :weight bold))
        ("PROG" . (:foreground "#009688" :weight bold))
        ("WAIT" . (:foreground "#7b1fa2" :weight bold))
        ("HOLD" . (:foreground "#78909c" :weight bold))
        ("CANCEL" . (:foreground "#d32f2f" :weight bold))))

(setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
                           (?B . (:foreground "yellow"))
                           (?C . (:foreground "green"))))

(setq org-hide-emphasis-markers t)

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c a") (kbd "C-u C-u C-u TAB"))))
(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c <up>") 'org-priority-up)))
(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c <down>") 'org-priority-down)))
(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-<return>") (kbd "C-e C-m"))))

(custom-theme-set-faces 'user
	`(org-level-1
	  ((t (:foreground "#97A7C8" :weight normal))))
	`(org-level-2
	  ((t (:foreground "#F4F4FF" :weight normal))))
	`(org-level-3
	  ((t (:foreground "#FFDD35" :weight normal))))
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

(setq alert-default-style 'libnotify)
(setq org-alert-interval 300
      org-alert-notify-cutoff 10
      org-alert-notify-after-event-cutoff 10)

(provide 'fm-org)
