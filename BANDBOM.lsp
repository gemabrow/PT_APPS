(defun c:bandbom ()
                      

   ;PROGRAM TO READ ATTRIBUTE FILE (C:\ACADWIN\CCS\CHAIRBOM.TXT)
   ;AND TO WRITE A CHAIR BILL-OF-MATERIALS BACK TO DRAWING

;EXTRACT ATTRIBUTES
(setvar "FILEDIA" 0) 
(prompt "Please wait for processing of attributes")(terpri)
(setvar "CMDECHO" 0)
(command "attext" "" "c:\\apps\\PT_CAD\\bom\\bandtem.txt" 
         "c:\\apps\\bandbom.txt")
(setvar "FILEDIA" 1)
(prompt "Please wait")(terpri)
(setq a (open "c:\\apps\\bandbom.txt" "r"))
(setq scale (getvar "dimscale"))
(setq alist '())
(setq bll '())  ; bll - BAND LENGTH LIST
(listatt)
(setq sp (getpoint "Pick starting point for band support bar schedule: "))
(printatt (length slist) 0 sp)
(setvar "CMDECHO" 1)
)

(defun listatt ()
   (while (setq l (read-line a))  
      ;(setq cl (proline l 1 nil nil ""))
      (if (= (substr l 2 6) "BNDSUP")
         (progn
            (setq bl (strcat (substr l 8 2) "\""))   ; bl - BAND LENGTH
            (if (not (member bl bll))
                (progn
                   (setq bll (cons bl bll))
                   (setq alist (cons (list bl 1) alist))
                )
                (progn 
                   (setq old (assoc bl alist))
                   (setq new (append old '(1)))
                   (setq alist (subst new old alist))
                )
            )
         )
      )
   )
   (close a)
   (setq slist '())
   (foreach x alist   ;CONSOLIDATE alist (L 1 1 ...) INTO slist (L TOTAL)
      (setq slist (cons (list (car x) (apply '+ (cdr x))) slist)))
   (if (> (length slist) 1) 
      (setq slist (sortbyl slist 1 '() (atoi (substr (car (car slist)) 1 2)) 0))
   )  
)
     ;;;SORT SLIST BY BAND SUPPORT BAR LENGTH
;sl - SLIST, n - COUNTER, rl - RETURN LIST, cv - CURRENT VALUE, cn - CURRENT VALUE COUNTER
(defun sortbyl (sl n rl cv cn / tv)
   (if (< n (length sl))
      (setq tv (atoi (substr (car (nth n sl)) 1 2)))) ;TRY VALUE      
   (cond  ((= (length sl) 0)
           (reverse rl)
          )
          ((= n (length sl))
           (setq cl (nth cn sl)) 
           (setq rl (cons cl rl))  ;ADD CURRENT LIST TO RESULT LIST
           (setq sl (append (reverse (cdr (member cl (reverse sl)))) (cdr (member cl sl)))) ;DELETE CURRENT LIST FROM SL                       
           (sortbyl sl 1 rl (atoi (substr (car (car slist)) 1 2)) 0)
          )
          ((< tv cv)                  
           (sortbyl sl (1+ n) rl tv n)
          )
          (T (sortbyl sl (1+ n) rl cv cn)
          )
   )
)  
 
(defun printatt (n i sp)
   (setq l (car (nth i slist)))
   (command "text" sp "" l)
   (setq l (itoa (cadr (nth i slist))))
   (setq np (list (+ (car sp) (* scale 0.75)) (cadr sp)))
   (command "text" np "" l)
   (setq sp (list (car sp) (- (cadr sp) (/ scale 3))))
   (setq i (1+ i))
   (if (< i n) (printatt n i sp))
)

