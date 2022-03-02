;; (setq gc-cons-threshold 100000000)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-hook 'prog-mode-hook 'global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(show-paren-mode 1)
(delete-selection-mode 1)

(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c r") 'recompile)
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "M-p") 'scroll-down-line)

(defun rc/jump-to-newline ()
  (interactive)
  (end-of-line)
  (newline))
(defun rc/jump-to-newline-prog-mode ()
  (interactive)
  (rc/jump-to-newline)
  (indent-for-tab-command))
(add-hook 'prog-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-<return>") 'rc/jump-to-newline-prog-mode)))
(add-hook 'text-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-<return>") 'rc/jump-to-newline)))

(defun rc/copy-line ()
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank))
(global-set-key (kbd "C-,") 'rc/copy-line)

(defun rc/duplicate-line ()
  (interactive)
  (rc/copy-line)
  (rc/jump-to-newline)
  (yank))
(global-set-key (kbd "C-.") 'rc/duplicate-line)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

(add-hook 'after-init-hook 'global-company-mode)

(setq tramp-auto-save-directory "/tmp")

(setq-default show-trailing-whitespace t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defadvice compile (around split-horizontally activate)
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    ad-do-it))

(defun rc/enlarge-window ()
  (interactive)
  (let ((size 15))
    (enlarge-window size)))
(global-set-key (kbd "C-x w") 'rc/enlarge-window)

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

(require 'multiple-cursors)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m t") 'mc/mark-all-like-this)

(require 'magit)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log)

(require 'treemacs)
(global-set-key (kbd "C-c t") 'treemacs)

(require 'vi-tilde-fringe)
(add-hook 'prog-mode-hook 'vi-tilde-fringe-mode)

(global-set-key (kbd "C-c C-e") 'eshell)
(global-set-key (kbd "M-o") 'other-window)

;; (setq inhibit-startup-screen t)

(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-\"") 'avy-goto-char-2)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g a") 'avy-goto-word-0)
(setq avy-highlight-first t)

(load-theme 'gruber-darker t)

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

(add-to-list 'after-init-hook
          (lambda ()
            (message (concat "emacs ("
			     (number-to-string (emacs-pid)) ") started in "
			     (emacs-init-time)))))

(load "~/.emacs.d/rc/org.el")
