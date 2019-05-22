(defun c:bandbom ()
                      

  ;PROGRAM TO READ ATTRIBUTE FILE (C:\ACADWIN\CCS\CHAIRBOM.TXT)
  ;AND TO WRITE A CHAIR BILL-OF-MATERIALS BACK TO DRAWING
  (setq osmode (getvar 'osmode))
  (setq cmdecho (getvar 'cmdecho))
  (setq filedia (getvar 'filedia))
  
  (setvar "FILEDIA" 0) 
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  
  ;SET OUTPUT PATH
  (setq current_dir (getvar 'Dwgprefix))
  (setq bandbom_output (strcat current_dir "\bandbom.txt"))
  ;EXTRACT ATTRIBUTES
  (command "attext" "" "c:\\apps\\PT_CAD\\bom\\bandtem.txt" bandbom_output)
  
  ;READ EXTRACTED ATTRIBUTES
  (setvar "FILEDIA" 1)
  (prompt "Please wait")(terpri)
  (setq a (open bandbom_output "r"))
  (setq scale (getvar "dimscale"))
  (setq alist '())
  (setq bll '())  ; bll - BAND LENGTH LIST
  (listatt)
  (setq sp (getpoint "\nPick starting point for band support bar schedule: "))
  (printatt (length slist) 0 sp)
  (setvar "CMDECHO" cmdecho)
  (setvar "FILEDIA" filedia)
  (setvar "OSMODE"  osmode))

(defun listatt ()
   (setq char_delimiter " ")
   (while (setq l (read-line a))
      ;(setq cl (proline l 1 nil nil ""))
      (if (= (substr l 2 6) "BNDSUP")
         (progn
            ;the substring we want is at the index of the delimiter + 1
            ;HOWEVER `substr` starts at 1 (as opposed to 0)
            ;THUSLY, we remedy this by adding 2 to the index of the delimiter
            (setq index (+ (vl-string-position (ascii char_delimiter) l) 2))
            (setq bl (strcat (substr l index) "\""))   ; bl - BAND LENGTH
            (if (not (member bl bll))
                (progn
                   (setq bll (cons bl bll))
                   (setq alist (cons (list bl 1) alist)))
                (progn 
                   (setq old (assoc bl alist))
                   (setq new (append old '(1)))
                   (setq alist (subst new old alist)))))))
   (close a)
   (setq slist '())
   (foreach x alist   ;CONSOLIDATE alist (L 1 1 ...) INTO slist (L TOTAL)
      (setq slist (cons (list (car x) (apply '+ (cdr x))) slist)))
   (if (> (length slist) 1) 
      (setq slist (sortbyl slist 1 '() (atoi (substr (car (car slist)) 1 2)) 0))))
     ;;;SORT SLIST BY BAND SUPPORT BAR LENGTH

;sl - SLIST, n - COUNTER, rl - RETURN LIST, cv - CURRENT VALUE, cn - CURRENT VALUE COUNTER
(defun sortbyl (sl n rl cv cn / tv)
   ;(princ (length (car (nth n sl))))
   (if (< n (length sl))
          (setq tv (atoi (substr (car (nth n sl)) 1 2))) ;TRY VALUE
   (cond  ((= (length sl) 0)
           (reverse rl))
          ((= n (length sl))
           (setq cl (nth cn sl)) 
           (setq rl (cons cl rl))  ;ADD CURRENT LIST TO RESULT LIST
           (setq sl (append (reverse (cdr (member cl (reverse sl)))) (cdr (member cl sl)))) ;DELETE CURRENT LIST FROM SL                       
           (sortbyl sl 1 rl (atoi (substr (car (car slist)) 1 2)) 0))
          ((< tv cv)                  
           (sortbyl sl (1+ n) rl tv n))
          (T (sortbyl sl (1+ n) rl cv cn))))  
 
(defun printatt (n i sp)
   (setq l (car (nth i slist)))
   (command "text" sp "" l)
   (setq l (itoa (cadr (nth i slist))))
   (setq np (list (+ (car sp) (* scale 0.75)) (cadr sp)))
   (command "text" np "" l)
   (setq sp (list (car sp) (- (cadr sp) (/ scale 3))))
   (setq i (1+ i))
   (if (< i n) (printatt n i sp)))

