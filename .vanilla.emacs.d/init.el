;; Package repository configuration
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
  '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)


;; adding fm directory to load-path for require
(add-to-list 'load-path (expand-file-name "fm" user-emacs-directory))


(require 'fm-editor)
(require 'fm-keymap)
(require 'fm-org)
(require 'fm-lang)
(require 'fm-modeline)
(require 'fm-dashboard)
(require 'fm-lsp)

