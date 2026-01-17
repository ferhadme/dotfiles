;;; fm-editor.el --- General editor configurations  -*- lexical-binding: t; -*-

;; Optimizing Emacs startup. The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 990 990))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;; Usual Emacs tweaks
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(show-paren-mode 1)
(delete-selection-mode 1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(setq tramp-auto-save-directory "/tmp")

(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)


;; Font and Theme
(set-frame-font "Fira Mono-12" nil t)

;; Theme from https://github.com/rexim/gruber-darker-theme
(load-theme 'gruber-darker t)
;; (load-theme 'doom-tomorrow-night t)


;; Indicating spaces with mark
(global-whitespace-mode)
(setq global-whitespace-display-mappings '((space-mark 32 [183] [46])))
(setq whitespace-space '(:foreground "#425872"))
(setq whitespace-style '(face spaces space-mark))


;; Tildes on empty lines like in Vim
(require 'vi-tilde-fringe)
(add-hook 'prog-mode-hook 'vi-tilde-fringe-mode)


;; Always start Emacs in fullscreen mode and make opacity 99%
(set-frame-parameter (selected-frame) 'alpha '(99 . 99))
(add-to-list 'default-frame-alist '(alpha . (99 . 99)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Enable relative line numbers for some modes
(dolist (mode '(text-mode-hook
		prog-mode-hook
		org-mode-hook
		conf-mode-hook))
  (add-hook mode (lambda ()
		   (display-line-numbers-mode 1)
		   (setq display-line-numbers-type 'relative))))

;; Override some modes which derive from the above
(dolist (mode '(term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Improve scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)


;; Compilation and shell
(defadvice compile (around split-horizontally activate)
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    ad-do-it))

(setq compilation-ask-about-save nil)

(setq eshell-prompt-function
      (lambda nil
	"λ "))
(setq eshell-prompt-regexp "λ ")


;; Company code completion
(add-hook 'after-init-hook 'global-company-mode)

;; Ido completion
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; Highlight todo keyword faces
(require 'hl-todo)
(global-hl-todo-mode 1)

(setq hl-todo-keyword-faces
      '(("TODO"       . "#20E3B2")
        ("FIXME"      . "#FF6C6B")
        ("BUG"        . "#FF4500")
        ("NOTE"       . "#51AFEF")
        ("OPTIMIZE"   . "#C678DD")
        ("DEPRECATED" . "#5B6268")
        ("HACK"       . "#ECBE7B")
        ("REVIEW"     . "#A9A1E1")))


;; Taken from https://github.com/stapelberg/configfiles
;; Store backups and auto-save files in a single directory so that
;; they don’t clutter up my filesystem (or fail to be written on curlftpfs):
(let ((backupdir (format "%s/emacs-backups%d/" (or (getenv "XDG_RUNTIME_DIR") "/tmp") (user-uid))))
  (mkdir backupdir t)
  (setq backup-directory-alist `(("." . ,backupdir)))
  (setq auto-save-file-name-transforms
	`((".*" ,backupdir t))))


;; Used snippets are from https://github.com/doomemacs/snippets
(add-to-list 'load-path
             "~/dotfiles/.vanilla.emacs.d/snippets/")
(require 'yasnippet)
(yas-global-mode 1)


;; Don't ask symbolic link to Git controlled source file
(setq vc-follow-symlinks t)


;; Scheme mode for Racket files
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

(provide 'fm-editor)
