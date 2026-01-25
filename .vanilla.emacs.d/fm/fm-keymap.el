;;; fm-keymap.el --- Custom keybindings  -*- lexical-binding: t; -*-

(require 'fm-helpers)

;; C-c keymap prefix is used for custom keybindings:
;; C-c c: Compilation
;; C-c o: Overwrite mode
;; C-c w: Opening workspaces
;; C-c m: Multiple cursors
;; C-c g: Magit git interface
;; C-c e: Eglot LSP client
;; C-c u: Undo tree

;; Compilation
(global-set-key (kbd "C-c c e") 'eshell)
(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c r") 'recompile)


;; Scrolling
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "M-p") 'scroll-down-line)

;; Some modes like man-mode overwrites global scrolling keybindings
(add-hook 'Man-mode-hook
          (lambda ()
	    (local-set-key (kbd "M-n") #'scroll-up-line)
	    (local-set-key (kbd "M-p") #'scroll-down-line)))


;; Window
(global-set-key (kbd "M-o") 'other-window)

;; Rebinding arrow keys for fast window switch
(global-set-key (kbd "<up>") 'windmove-up)
(global-set-key (kbd "<down>") 'windmove-down)
(global-set-key (kbd "<left>") 'windmove-left)
(global-set-key (kbd "<right>") 'windmove-right)


;; Text editing
(global-set-key (kbd "C-<return>") (kbd "C-e C-m"))

(global-set-key (kbd "C-,") 'fm/copy-line)
(global-set-key (kbd "C-.") 'fm/duplicate-line)
(global-set-key (kbd "C-x w") 'fm/enlarge-window)

(global-set-key (kbd "C-c o") 'overwrite-mode)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Smex: A M-x enhancement built on top of Ido
(fm/require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)


;; Workspaces
(global-set-key (kbd "C-c w p") (lambda () (interactive) (find-file "~/Programming/")))
(global-set-key (kbd "C-c w o") (lambda () (interactive) (find-file "~/Documents/Org/")))
(global-set-key (kbd "C-c w c") (lambda () (interactive) (find-file "~/dotfiles/")))


;; Editing in many places at once
(fm/require 'multiple-cursors)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m t") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)


;; A Git interface for Emacs
(fm/require 'magit)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log)


;; Jump to things in Emacs tree-style
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-\"") 'avy-goto-char-2)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g a") 'avy-goto-word-0)
(setq avy-highlight-first t)


;; Move current line or region up or down
(fm/require 'move-text)
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; Emacs Eglot LSP client
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c e f") 'xref-find-references)))

;; Map CMD to Control (for MacOS)
(setq mac-command-modifier 'control)

;; Undo Tree visualizer
(fm/require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-c u v") 'undo-tree-visualize)

(provide 'fm-keymap)
