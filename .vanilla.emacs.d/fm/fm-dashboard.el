;;; fm-dashboard.el --- dashboard customization on top of grid and enlight  -*- lexical-binding: t; -*-

(require 'fm-helpers)

(fm/require 'enlight)

(setopt initial-buffer-choice #'enlight)

;; from https://www.asciiart.eu/art/c4ea3391b2d27515
(defvar bison-ascii
  (propertize
    (concat
"             _.-````'-,_\n"
"   _,.,_ ,-'`           `'-.,_\n"
" /)     (\                   '``-.\n"
"((      ) )                      `\\\n"
" \\)    (_/                        )\\\n"
"  |       /)           '    ,'    / \\\n"
"  `\\   ^'            '     (    /  ))\n"
"    |      _/\ ,     /    ,,`\   (  )`\n"
"     \\Y,   |  \\  \\  | ````| / \\_ \\\n"
"       `)_/    \\  \\  )    ( >  ( >\n"
"                \\( \\(     |/   |/\n"
"               /_(/_(    /_(  /_(\n")

    'face 'enlight-yellow-bold))

bison-ascii

(setq enlight-content
  (concat
    bison-ascii
    "\n"
    (propertize "MENU" 'face 'highlight)
    "\n"
    (enlight-menu
      '(("Agenda"
          ("Current day" (org-agenda nil "a") "a")
          ("Todo" (org-agenda nil "t") "t"))
         ("Source"
           ("Programming" (dired "~/Programming/") "P")
           ("Configuration" (dired "~/dotfiles/") "C")
           ("Temp" (dired "~/Temp/") "T"))))))


(provide 'fm-dashboard)
