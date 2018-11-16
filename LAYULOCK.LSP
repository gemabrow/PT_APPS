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
; on 07/03/92, will allow you to pick a Layer(s) to be unlocked.
 (defun C:LAYULOCK () ;(/ ss ssl cl laylist c Lenstr laylstst clayroff tm)
   (prompt "\nPick entity(s) on layer(s) to be locked: ")
   (setq ss (ssget))
   (setq ssl (sslength ss)); selection set length
   (setq laylist nil)
   (setq c 0); counter
   (repeat ssl
      (setq en (ssname ss c))
      (setq lname (cdr (assoc 8 (entget en))))
      (setq laylist (cons lname laylist))
      (setq c (+ c 1))
   );repeat
   (setq laylstst ""); layer list in string form
   (setq c 0) ; counter
   (while (< c ssl)
      (setq laylstst (strcat laylstst  "," (nth c laylist)))
      (setq c (+ c 1))
   );while
   (setq Lenstr (strlen laylstst))
   (setq laylstst (substr laylstst 2 (- Lenstr 1))) ; strip first comma
   (command "layer" "UNLOCK" laylstst "")
);defun C:LAYULOCK
