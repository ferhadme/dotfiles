;;; fm-lsp.el --- LSP configuration  -*- lexical-binding: t; -*-

(require 'fm-helpers)

(add-hook 'after-init-hook 'global-company-mode)

(fm/require 'eglot)

;; No auto-docs in minibuffer
(setq eldoc-idle-delay 10.0)

(defun fm/eglot-silent-ui ()
  "Disable Flymake and other heavy UI elements."
  (flymake-mode -1))

(add-hook 'eglot-managed-mode-hook #'fm/eglot-silent-ui)

;; Languages to use with LSP server (backends like Clang, Elisp are supported by company out of box)
(defvar fm/lsp-languages
  '(rust-mode
     ;; Additions
     )
  "List of major modes to enable Eglot for.")

(dolist (hook fm/lsp-hooks)
  (add-hook hook #'eglot-ensure))

(provide 'fm-lsp)
