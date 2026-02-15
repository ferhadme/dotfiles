;;; fm-lang.el --- General coding and language specific configurations  -*- lexical-binding: t; -*-

;; Replace tabs with spaces everywhere
(setq-default indent-tabs-mode nil)

;; In case of some file contains tab, indent it with 4 by default
(setq-default tab-width 4)

;; CC-mode offset
(setq-default c-basic-offset 4)

;; Use block comments over line ones
(add-hook 'c-mode-hook
  (lambda ()
    (c-toggle-comment-style 1)))

;; Emacs Lisp indentation
(setq-default lisp-indent-offset 2)

;; 2 space width for html and css modes
(setq-default css-indent-offset 2)
(setq-default sgml-basic-offset 2)

;; Scheme mode for Racket files
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

(provide 'fm-lang)
