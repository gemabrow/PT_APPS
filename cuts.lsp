(defun c:cuts ()
                      

   ;(if (findfile "c:\\bom.txt")
   ;   (command "shell" "del c:\\bom.txt")
   ;)
   
                 ;EXTRACT TENDON ATTRIBUTES
   (command "attext" "c" "c:/apps/PT_CAD/bom/tendtem.txt" "c:/bom.txt")
   (while (not (findfile "c:/bom.txt"))
   )  
   (setq pf (command "_-VBARUN" "cuts.cutout"))                       
)   
         