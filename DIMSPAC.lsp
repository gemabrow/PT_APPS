(defun c:dimspac ()
                     

   (setq dl (list '("H" "hor") '("V" "vert") '("A" "aligned")))
   (initget "h H v V a A")
   (setq mode (strcase (getkword "Enter H, V or A for hor, vert or aligned dimensions: ")))
   (setq mode (cadr (assoc mode dl)))
   (setq ns (getint "\nEnter number of spaces: "))
   (setq p1 (getpoint "\nPick dimension location: "))
   (while (eval 'p1)
     (setq p2 (getpoint "\nPick first point for space dimension: "))
     (setq p3 (getpoint "\nPick second point for space dimension: "))
     (cond ((= mode "hor")
            (setq d (distance p2 (list (car p3) (cadr p2))))
           )
           ((= mode "vert")
            (setq d (distance p2 (list (car p2) (cadr p3))))
           )
           (T
            (setq d (distance p2 p3))
           )
     )
     (setq s (rtos (/ d ns) 4 3))
     (cond ((> (- d (* ns (distof s 4))) 0.1)
               (setq off "(+)")
            )
            ((> (- (* ns (distof s)) d) 0.1)
             (setq off "(-)")
            )
            (T (setq off ""))
      )
      (setq sdim (strcat (itoa ns) " SPA @ " 
                         s off)                               
      )
      (command "dim" mode p2 p3 p1 sdim "e")
      (setq p1 (getpoint "\nPick dimension location, return to exit: "))
   )      
)    
  