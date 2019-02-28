; ********************************************************************
;                           DELLAYER.LSP
                            
;  This program deletes all entities on a specified layer.

; ******************************************************************** 

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

(defun C:DELLAYER (/ olderr ocmd L S)

   (setq olderr  *error*
         *error* dellerr)
   (setq ocmd (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)
;   (setq L (strcase (getstring "\nLayer to delete: ")))
    (prompt "\nPick an entity on the layer to be deleted: ")
    (setq ent (entsel))
    (setq entlist (entget (car ent)))
    (setq L (cdr (assoc 8 entlist)))
   (setq S (ssget "X" (list (cons 8 L))))  ; Get all entities on layer
   (if S
      (command "ERASE" S "")          ; Delete 'em!
      (princ "Layer empty or not a valid layer name.")
   )
   (setq S nil)                       ; Free selection-set
   (setvar "CMDECHO" ocmd)            ; Restore saved mode
   (setq *error* olderr)              ; Restore old *error* handler
   (princ)
)

;This program lets you change a group of entities to another layer
;by simply pointing to any entity on the new layer.
  (defun c:chglayer (/ f6 f7 n i g7 i7 g6 i6 g8)
    (prompt "\nSelect entities to be changed: ")
    (setq f6 (ssget))
    (prompt "\nPoint to entity on target layer: ")
    (setq f7 (entsel))
    (setq n (sslength f6))
    (setq i 0)
    (setq g7 (entget (car f7)))
    (setq i7 (assoc 8 g7))
    (repeat n
       (setq g6 (entget (ssname f6 i)))
       (setq i6 (assoc 8 g6))
       (setq g8 (subst i7 i6 g6))
       (entmod g8)
       (setq i (1+ i))
     )
    (princ "\nFinished!")
    (princ)
  )

; This program sets you to a layer by letting you point to 
; another entity that's on your target layer.

(defun C:sellayer (/ en ed lname)
    (prompt "\nPick an entity on the target layer: ")
    (setq en (entsel))
    (setq ed (entget (car en)))
    (setq lname (cdr (assoc 8 ed)))
    (command "layer" "s" lname "")
    (princ)
);defun sellayer


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

; The following program, written by Ken Callaham of Universal Data Consultants
; on 10/01/90, will allow you to pick a Layer(s) to remain on.  All others
; will be turned off

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

(defun C:LAYALLON (/ tm)
   (setq tm (getvar "tilemode"))
   (if (= tm 0) (command "vplayer" "thaw" "*" "" ""))
   (command "layer" "thaw" "*" "")
);defun C:layallon

; The following program, written by Ken Callaham of Universal Data Consultants
; on 07/03/92, will allow you to pick a Layer(s) to be locked.
 (defun C:LAYLOCK () ;(/ ss ssl cl laylist c Lenstr laylstst clayroff tm)
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
   (command "layer" "LOCK" laylstst "")
);defun C:LAYLOCK

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
