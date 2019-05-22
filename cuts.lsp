(defun c:cuts ()
  ;(if (findfile "c:\\bom.txt")
  ;   (command "shell" "del c:\\bom.txt")
  ;)
  ;SET OUTPUT PATH
  (setq current_dir (getvar 'Dwgprefix))
  (setq bom_output (strcat current_dir "\bom.txt"))
  ;EXTRACT ATTRIBUTES
  (command "attext" "c" "c:\\apps\\PT_CAD\\bom\\tendtem.txt" bom_output)
  
  (while (not (findfile bom_output)))  
    (setq pf (command "_-VBARUN" "cuts.cutout")))