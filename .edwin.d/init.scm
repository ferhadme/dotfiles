(define EDWIN-IS-IN-WINDOW (not (member 'console create-editor-args)))

(cond (EDWIN-IS-IN-WINDOW
       ((ref-command set-foreground-color) "white")
       ((ref-command set-background-color) "#1d1f28")
       ((ref-command set-mouse-color) "white")
       ((ref-command set-cursor-color) "#90e0ef")
       ((ref-command set-font) "10x20")
       ((ref-command set-frame-size) 85 40)
       ((ref-command set-frame-position) 50 100))
      (else '()))
