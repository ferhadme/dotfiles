(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode nil)
 '(cua-normal-cursor-color "dark gray")
 '(custom-enabled-themes '(manoj-dark))
 '(display-line-numbers 'relative)
 '(dynamic-completion-mode nil)
 '(fringe-mode 0 nil (fringe))
 '(global-subword-mode t)
 '(global-visual-line-mode t)
 '(ido-create-new-buffer 'prompt)
 '(ido-mode 'both nil (ido))
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(doom-themes yasnippet-snippets multiple-cursors racket-mode))
 '(ps-line-number-color '(244 197 77))
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 4)
 '(scroll-bar-mode t)
 '(show-paren-highlight-openparen t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#181818" :foreground "WhiteSmoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "CTDB" :family "Fira Mono"))))
 '(cursor ((t (:background "white"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "green"))))
 '(font-lock-comment-face ((t (:foreground "green" :slant oblique))))
 '(font-lock-doc-face ((t (:foreground "purple" :slant oblique))))
 '(font-lock-keyword-face ((t (:foreground "#e80067"))))
 '(font-lock-preprocessor-face ((t (:foreground "CornFlowerBlue" :slant italic))))
 '(font-lock-type-face ((t (:foreground "#f38c40"))))
 '(highlight ((t (:background "#4e4a4a" :foreground "#1B2229"))))
 '(line-number ((t (:inherit (shadow default) :foreground "#f3c44d"))))
 '(mc/region-face ((t (:inherit region :background "black"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(mode-line-buffer-id ((t (:background "black" :foreground "#d290c9" :weight bold :height 0.9))))
 '(mode-line-highlight ((t (:inherit highlight :box (:line-width 2 :color "grey40" :style released-button)))))
 '(org-level-3 ((t (:foreground "chocolate1"))))
 '(org-level-4 ((t (:foreground "Cyan1"))))
 '(region ((t (:extend t :background "#4e4a4a")))))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-RET") (kbd "C-e C-m"))
(global-set-key (kbd "C-c C-c") 'capitalize-region)
(global-set-key (kbd "C-k") 'kill-line)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Modifying scrolling
(defun next-line-and-recenter ()
  (interactive)
  (next-line)
  (recenter))

(defun previous-line-and-recenter ()
  (interactive)
  (previous-line)
  (recenter))

(global-set-key (kbd "<up>") 'previous-line)
(global-set-key (kbd "<down>") 'next-line)
(global-set-key (kbd "C-p") 'previous-line)
(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (quote [M-down]) (quote scroll-up-line))
(global-set-key (quote [M-up]) (quote scroll-down-line))
(global-set-key (kbd "C-c c") 'compile)

;; IntelliJ IDEA style move-line-up and move-line-down
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))
(global-set-key [(control shift up)] 'move-line-up)

(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))
(global-set-key [(control shift down)] 'move-line-down)

;; overwrite selected text
(delete-selection-mode t)

;; org-mode
(setq org-agenda-files (list "~/Documents/org/day.org"))
(setq org-log-done 'note)
                      
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-set-style "k&r")
(setq c-basic-offset 4)

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

(global-set-key [C-S-left] 'shift-left)
(global-set-key [C-S-right] 'shift-right)

;;
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        ))

(yas-global-mode 1)

;; basic doom-one theme configuration
(require 'doom-themes)
;; Global settings (defaults)
(setq doom-themes-enable-bold t 
      doom-themes-enable-italic t)
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
