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
;; C-c p: Consult functions


;; Compilation
(global-set-key (kbd "C-c c e") 'eshell)
(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c r") 'recompile)

(global-set-key (kbd "C-c r") 'revert-buffer)


;; Scrolling
(global-set-key (kbd "M-n") #'scroll-up-line)
(global-set-key (kbd "M-p") #'scroll-down-line)

;; Some modes like man, markdown mode overwrites global scrolling keybindings
(defun fm/custom-scrolling ()
  (local-set-key (kbd "M-n") #'scroll-up-line)
  (local-set-key (kbd "M-p") #'scroll-down-line))

(dolist (hook '(Man-mode-hook markdown-mode-hook))
  (add-hook hook #'fm/custom-scrolling))


;; Window
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x w") #'fm/enlarge-window)


;; Text editing
(global-set-key (kbd "C-<return>") (kbd "C-e C-m"))

(global-set-key (kbd "C-,") #'fm/copy-line)
(global-set-key (kbd "C-.") #'fm/duplicate-line)

(global-set-key (kbd "C-c o") 'overwrite-mode)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Smex: A M-x enhancement built on top of Ido
(fm/require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)


;; Consult: Search and navigation with live preview
(global-set-key (kbd "C-c p m") 'consult-imenu)
(global-set-key (kbd "C-c p o") 'consult-outline)
(global-set-key (kbd "C-c p b") 'consult-buffer)
(global-set-key (kbd "C-c p p") 'consult-project-buffer)
(global-set-key (kbd "C-c p y") 'consult-yank-from-kill-ring)

(global-set-key (kbd "C-c p g") 'consult-yank-from-kill-ring)

;; Use M-g M-* prefix for goto-* and search functions
(global-set-key (kbd "M-g M-l") 'consult-goto-line)
(global-set-key (kbd "M-g M-s") 'consult-line)


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
(fm/require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "M-g l") 'avy-goto-line)
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
