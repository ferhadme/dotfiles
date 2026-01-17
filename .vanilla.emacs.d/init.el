;; Package repository configuration
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)


;; adding fm directory to load-path for require
(add-to-list 'load-path (expand-file-name "fm" user-emacs-directory))

;; loads fm/**
(require 'fm-editor)
(require 'fm-keymap)
(require 'fm-org)


;; Tab width configuration for major modes
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)
                         ;; Use spaces instead of tabs
                         (setq c-basic-offset 4
                               tab-width 4
                               indent-tabs-mode nil)))

;; For other modes
(defun tab-width-config ()
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil))

(add-hook 'go-mode-hook 'tab-width-config)
(add-hook 'makefile-mode-hook 'tab-width-config)
(add-hook 'conf-mode-hook 'tab-width-config)
(add-hook 'rust-mode-hook 'tab-width-config)
(add-hook 'perl-mode-hook 'tab-width-config)
(add-hook 'js-mode-hook 'tab-width-config)
