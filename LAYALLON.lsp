(defun dellerr (s)                    ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
   (if (/= s "Function cancelled")
       (princ (strcat "\nError: " s))
   )
   (setq S nil)                       ; Free selection-set if any
   (setvar "CMDECHO" ocmd)            ; Restore saved mode
   (setq *error* olderr)              ; Restore old *error* handler
   (princ)
)

(defun C:LAYALLON (/ tm)
   (setq tm (getvar "tilemode"))
   (if (= tm 0) (command "vplayer" "thaw" "*" "" ""))
   (command "layer" "thaw" "*" "")
);defun C:layallon
