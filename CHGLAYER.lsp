
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
