
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


(defun C:LAYERON (/ ss ssl cl laylist c Lenstr laylstst tm)
   (prompt "\nSelect entities on layers to remain on: ")
   (setq ss (ssget))
   (setq ssl (sslength ss)); selection set length
   (setq cl (getvar "clayer"))
   (setq laylist nil)
   (setq c 0); counter
   (while (< c ssl)
      (setq en (ssname ss c))
      (setq lname (cdr (assoc 8 (entget en))))
      (setq laylist (cons lname laylist))
      (setq c (+ c 1))
   );while
   (setq tm (getvar "tilemode"))
   (if (= tm 0)
      (command "vplayer" "freeze" "*" "" "")
      (command "layer" "freeze" "*" "")
   );if
   (setq c 0); counter
   (setq laylstst ""); layer list in string form
   (while (< c ssl)
      (setq laylstst (strcat laylstst  "," (nth c laylist)))
      (setq c (+ c 1))
   );while
   (setq Lenstr (strlen laylstst))
   (setq laylstst (substr laylstst 2 (- Lenstr 1))) ; strip first comma
   (if (= tm 0)
      (command "vplayer" "thaw" laylstst "" "")
      (command "layer" "thaw" laylstst "")
   );if
);defun C:VPLAYON
