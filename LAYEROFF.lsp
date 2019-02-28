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

; The following program, written by Ken Callaham of Universal Data Consultants
; on 10/01/90, will allow you to pick a Layer(s) to be turned off.
 (defun C:LAYEROFF (/ ss ssl cl laylist c Lenstr laylstst clayroff tm)
   (prompt "\nPick entity(s) on layer(s) to be turned off: ")
   (setq ss (ssget))
   (setq ssl (sslength ss)); selection set length
   (setq cl (getvar "clayer"))
   (setq laylist nil)
   (setq c 0); counter
   (setq clflag 0) ; current layer flag
   (repeat ssl
      (setq en (ssname ss c))
      (setq lname (cdr (assoc 8 (entget en))))
      (if (/= cl lname)  ; if user did not select current layer
         (progn   ; then
            (setq laylist (cons lname laylist))
         );progn
         (progn   ; else
            (setq clflag 1)
         );progn
      );if
      (setq c (+ c 1))
   );repeat
   (setq laylstst ""); layer list in string form
   (setq c 0) ; counter
   (if (/= clflag 0) (setq ssl (- ssl 1))) ; reduce size of selection
;   set if user selected current layer as layer to be frozen
   (while (< c ssl)
      (setq laylstst (strcat laylstst  "," (nth c laylist)))
      (setq c (+ c 1))
   );while
   (setq clayroff (member cl laylist)); see if current layer is to be turned off
   (setq Lenstr (strlen laylstst))
   (setq laylstst (substr laylstst 2 (- Lenstr 1))) ; strip first comma
   (setq tm (getvar "tilemode"))
   (if (= tm 0)
      (command "vplayer" "freeze" laylstst "" "")
      (command "layer" "freeze" laylstst "")
   );if
;   (if (/= clayroff nil) (command "vplayer" "freeze" laylstst "y" "")  (command "vplayer" "freeze" laylstst "C" ""))
);defun C:vplayfrz

