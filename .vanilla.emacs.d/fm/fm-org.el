;;; fm-org.el --- Org configuration  -*- lexical-binding: t; -*-

;; Todo keywords faces
(setq org-todo-keywords
      '((sequence "TODO(t)" "PROG(p)" "HOLD(h)" "|" "DONE(d)" "CANCEL(c)")))

(setq org-todo-keyword-faces
      '(("PROG" . (:foreground "#009688" :weight bold))
        ("HOLD" . (:foreground "#78909c" :weight bold))
        ("CANCEL" . (:foreground "#d32f2f" :weight bold))))

;; Priority faces
(setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
                           (?B . (:foreground "yellow"))
                           (?C . (:foreground "green"))))

;; Hide formatting characters
(setq org-hide-emphasis-markers t)

;; Line soft wrapping, replacing * (bullets) with UTF-8 circles and local keybindings
(defun org-hook-tweaks ()
  (visual-line-mode)
  (org-bullets-mode 1)
  (local-set-key (kbd "C-c a") (kbd "C-u C-u C-u TAB"))
  (local-set-key (kbd "C-c <up>") 'org-priority-up)
  (local-set-key (kbd "C-c <down>") 'org-priority-down)
  (local-set-key (kbd "C-<return>") (kbd "C-e C-m")))

(add-hook 'org-mode-hook #'org-hook-tweaks)

;; Header level faces
(custom-theme-set-faces
 'user
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

;; Inline image link in buffer
(setq org-startup-with-inline-images t)

;; Ensures code inside #+BEGIN_SRC blocks is syntax-highlighted
(setq org-src-fontify-natively t)

(set-face-attribute 'italic nil
                :slant 'oblique
                :underline nil)

;; Notifications
(setq alert-default-style 'libnotify)
(setq org-alert-interval 300
      org-alert-notify-cutoff 10
  org-alert-notify-after-event-cutoff 10)

(setq org-agenda-files '("~/Documents/org/"))

(provide 'fm-org)
