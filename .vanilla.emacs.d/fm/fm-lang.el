;;; fm-lang.el --- General coding and language specific configurations  -*- lexical-binding: t; -*-

;; Replace tabs with spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Tab width configuration for C like languages
(defun fm/tab-width-config ()
  (setq c-basic-offset 4
    tab-width 4
    indent-tabs-mode nil))

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq tab-width 4)
    (setq indent-tabs-mode nil)
    (setq lisp-indent-offset 2)))

;; C
(add-hook 'c-mode-hook
  (lambda ()
    (interactive)
    (c-toggle-comment-style -1)
    (fm/tab-width-config)))

(add-hook 'go-mode-hook 'fm/tab-width-config)
(add-hook 'makefile-mode-hook 'fm/tab-width-config)
(add-hook 'conf-mode-hook 'fm/tab-width-config)
(add-hook 'rust-mode-hook 'fm/tab-width-config)
(add-hook 'perl-mode-hook 'fm/tab-width-config)
(add-hook 'js-mode-hook 'fm/tab-width-config)


;; Scheme mode for Racket files
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))


(provide 'fm-lang)
