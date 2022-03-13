(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)


(load "~/.emacs.d/fm/org.el")
(load "~/.emacs.d/fm/helpers.el")


;; Ido completion
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; Common configurations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(show-paren-mode 1)
(delete-selection-mode 1)
(setq visible-bell t)

(setq-default show-trailing-whitespace t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'prog-mode-hook 'global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(add-to-list 'after-init-hook
             (lambda ()
               (message (concat "emacs ("
				(number-to-string (emacs-pid)) ") started in "
				(emacs-init-time)))))

(setq tramp-auto-save-directory "/tmp")

(defadvice compile (around split-horizontally activate)
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    ad-do-it))

(setq inhibit-startup-screen t)

(setq eshell-prompt-function
      (lambda nil
	"λ "))

(global-set-key (kbd "C-c C-e") 'eshell)
(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c r") 'recompile)
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-o") 'other-window)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-<return>") 'rc/jump-to-newline-prog-mode)))
(add-hook 'text-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-<return>") 'rc/jump-to-newline)))
(global-set-key (kbd "C-,") 'rc/copy-line)
(global-set-key (kbd "C-.") 'rc/duplicate-line)
(global-set-key (kbd "C-x w") 'rc/enlarge-window)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Scheme mode for Racket files
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))


;; A M-x enhancement built on top of Ido
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)


;; C programming configurations
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

(add-hook 'makefile-mode-hook (lambda ()
				(setq tab-width 4)))


;; Company code completion
(add-hook 'after-init-hook 'global-company-mode)


;; Highlight todo keyword faces
(require 'hl-todo)
(global-hl-todo-mode 1)

(setq hl-todo-keyword-faces
      '(("TODO"   . "#ff07fe")
        ("FIXME"  . "#fc4f64")
	("BUG"  . "#FF0000")
        ("NOTE"  . "#f9ff61")
	("OPTIMIZE"  . "#7943f8")
        ("DEPRECATED" . "#848484")
        ("HACK"   . "#00ff64")
        ("REVIEW"   . "#1E90FF")))


;; Editing in many places at once
(require 'multiple-cursors)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m t") 'mc/mark-all-like-this)


;; A Git interface for Emacs
(require 'magit)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log)


;; A tree layout file explorer
(require 'treemacs)
(global-set-key (kbd "C-c t") 'treemacs)


;; Tildes on empty lines like in Vim
(require 'vi-tilde-fringe)
(add-hook 'prog-mode-hook 'vi-tilde-fringe-mode)


;; Jump to things in Emacs tree-style
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-\"") 'avy-goto-char-2)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g a") 'avy-goto-word-0)
(setq avy-highlight-first t)


;; Theme from https://github.com/rexim/gruber-darker-theme
(load-theme 'gruber-darker t)
