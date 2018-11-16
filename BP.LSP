(defun c:bmplan ()
                    
  
                   ;SET MAXIMUM SPACING
                     
                     (setq scale (getvar "dimscale"))
                     (setvar "cmdecho" 0)
    (if (= (cdr (assoc '40 (tblsearch "style" (getvar "textstyle")))) 0.0) ;RESET TEXT HT TO 1/8" IF CURRENT HT IS 0"
        (command "style" "ROMANS" "ROMANS.shx,SPECIAL.shx" (/ (getvar "dimscale") 8.0) 0.9 0 "N" "N" "N")  
    )
   
    (input) 
    (genip)          
    (drawbeam)
    (label)
    (dimspa)
    (drawtend)
    (command "ucs" "r" "OLD")    
    (command "style" "" "" "" 0.9 "" "" "" "")
    (setvar "filedia" 1)
    (if (/= omo nil)
       (setvar "osmode" omo)
    )
)     


(defun input ()
   (command "units" 4 8 1 2 0.0 "N")
   (setq maxspa (getreal "Enter maximum spacing between supports in inches [48]: "))
   (if (= maxspa nil)
      (setq maxspa 48.0)
   )
   (initget "1 2")
   (setq ntenlay (getkword "\nEnter 1 or 2 to designate number of tendon layers [2]: "))
   (if (= ntenlay nil)
      (setq ntenlay "2")
   )
   (setq ntenlay (atof ntenlay))
   (initget  "10 12 20 simp SIMP")
   (setq a1 "100") 
   (while (not (or (< (atoi a1) 50) (= (strcase a1) "H")))
      (setq a1 (getstring "\nEnter N value (L/N) to locate inflection point, 0 for simple parabola, H for HARPED [10]: "))
      (if (= a1  "") (setq a1 "10"))
   )
   (if (/= (strcase a1) "H")
       (progn
          (setq a1 (atof a1))
          (if (/= a1 0.0)        
             (setq a1 (/ 1.0 a1))
          )
       )
   )
   (setq beamwidth (getreal "Enter beam width in inches: "))
   (setq ncgs 1)

   (setq nspan (getint "Enter total number of complete spans in beam: "))
   (initget "L l R r B b")
   (setq cant (getkword "Enter L[left], R[right] or B[both] for location of cantilever or return if none: "))
   (if (not cant) (setq cant "N"))
   (initget "Y y N n")
   (setq lowlocf (getkword "Are all CGS lowpoints at midpoint of span?[Y or N] :"))
   (setq lowl nil)
   (setq span nil)
   (if (= (strcase lowlocf) "N")
      (if (= nspan 1)
	  (progn
	     (setq span 1)
	     (setq lowloc (getreal "Enter lowpoint location as fractional part of span [from left support- eg 0.4]: "))
             (setq lowl (list (list span lowloc)))
	  )
          (progn
             (setq span (getint "Enter span number for which lowpoint location is to be specified [from left begin w/ 1]: "))
             (setq lowloc (getreal "For this span enter lowpoint location as fractional part of span [from left support- eg 0.4]: "))
             (setq lowl (list (list span lowloc)))
          )
      )	    
   )
   (while (and (/= span nil) (> nspan 1))
       (setq span (getint "Enter next span number for which lowpoint location is to be specified [return if none]: ")) 
       (if (/= span nil)
          (progn
             (setq lowloc (getreal "Enter lowpoint location as fractional part of span [from left support- eg 0.4]: "))
             (setq lowl (cons (list span lowloc) lowl)) ;ASSOCIATION LIST
          )
       )
   )
                       ;ENTER SPAN & CANTILEVER LENGTHS  
   (setq el (getdist "Enter distance from leftmost cgs to end of beam: "))
   (setq distdata '())
   (if (or (= (strcase cant) "B")(= (strcase cant) "L"))
       (progn
          (setq sl (getdist "Enter cantilever distance from leftmost cgs to first support: "))
          (setq distdata (cons sl distdata))
       )
   )
   (if (= nspan 1)
      (setq alspan (getdist "Enter span length in feet & inches: "))
      (setq alspan (getdist "If all spans are of equal length enter span length in feet & inches, else return: "))
   ) 
   (setq n 1)
   (while (<= n nspan)
       (if (not alspan)
          (progn       
             (princ "\nEnter distance between supports at span no. ")(princ n)(princ ": ")
             (setq sl (getdist))
          )
          (setq sl alspan)
       )
       (if (setq f (cadr (assoc n lowl))) ;TEST FOR LOWPOINT NOT AT MIDSPAN
           (progn 
              (setq d (* sl f))
              (setq distdata (append (list (- sl d) d) distdata))
           )
           (progn 
              (setq d (* sl 0.5))
              (setq distdata (append (list d d) distdata))
           )
       ) 
       (setq n (1+ n))
   )
   (if (or (= (strcase cant) "B")(= (strcase cant) "R"))
       (progn
          (setq sl (getdist "Enter cantilever distance from last support to rightmost cgs: "))
          (setq distdata (cons sl distdata))
       )
   )  
   (setq rl (getdist "Enter distance from rightmost cgs to end of beam: "))
   (setq dendata (list el rl))
   (initget "ss SS sd SD ds DS")
   (setq endanch (getstring "Enter SS, SD or DS to denote configuration of tendon anchors as stressed or deadend at each end: "))
   (setq lanch (substr endanch 1 1))
   (setq ranch (substr endanch 2 1))
   (setq cgsn (1+ (* 2 nspan)))  ;COMPUTE NO. OF CGS'S
   (cond ((or (= (strcase cant) "R") (= (strcase cant) "L"))
          (setq cgsn (1+ cgsn))
         )
         ((= (strcase cant) "B")
          (setq cgsn (+ cgsn 2))
         )
   )
   (setq cgs (getdist "Enter height of beginning cgs 1 [leftmost CGS]: "))
   (setq cgsht (list cgs)) ;INITIALIZE CGS LIST
   (setq aphp nil allp nil) ;INITIALIZE GLOBAL HIGHPOINT & LOWPOINT CGS VARIABLES
   (if (not (and (= nspan 1) (= (strcase cant) "N")))
      (setq alhp (getdist "If all cgs highpoints are equal enter highpoint height, else return: "))
   )
   (if (= nspan 1)
      (setq allp (getdist "Enter lowpoint height: "))
      (setq allp (getdist "If all cgs lowpoints are equal enter lowpoint height, else return: "))
   )  
   (setq n 2)
   (if (or (= (strcase cant) "B")(= (strcase cant) "L"))
       (setq nextcgs " at highpoint: ")
       (setq nextcgs " at lowpoint: ")
   )
   (while (< n cgsn)  ;GENERATE CGSHT
      (if (= nextcgs " at highpoint: ")
         (progn            
            (if alhp
                (setq cgs alhp)
                (progn
                   (princ "\nEnter height of cgs ")(princ n)(princ nextcgs)
                   (setq cgs (getdist))
                )
            )
            (setq nextcgs " at lowpoint: ")
         )
         (progn            
            (if allp
               (setq cgs allp)
               (progn
                  (princ "\nEnter height of cgs ")(princ n)(princ nextcgs)
                  (setq cgs (getdist))
               )
            )
            (setq nextcgs " at highpoint: ")
         )      
      )
      (setq cgsht (cons cgs cgsht))      
      (setq n (1+ n))
   )
   (setq cgs (getdist "Enter height of last cgs: "))
   (setq cgsht (cons cgs cgsht))
       ;GENERATE CGSDATA 
   (setq cgsdata (list lanch))
   (if (or (= (strcase cant) "L") (= (strcase cant) "B"))
      (setq cgsdata (cons "H" cgsdata))     
   )
   (setq n 1)
   (while (<= n nspan)
      (setq cgsdata (append (list "H" "L") cgsdata))
      (setq n (1+ n))
   )
   (if (or (= (strcase cant) "R") (= (strcase cant) "B"))
      (setq cgsdata (cons ranch cgsdata))   ;ADD RIGHT STRESS OR DEADEND ANCHOR IF CANTILEVERED ON RIGHT
      (setq cgsdata (cons ranch (cdr cgsdata)))  ;IF NO CANTILEVER REPLACE "H" W/ "S" OR "D"
   )  
   (setq cgsdata (reverse cgsdata))
   (setq cgsht (reverse cgsht))
   (setq distdata (reverse distdata))
)
      
(defun genip ()
                  ;BEAM INSERTION POINTS
   (setq blength (+ (apply '+ dendata) (apply '+ distdata)))
   (setq bip (getpoint "Pick beam start point: ")) ;POINT IN STARTING UCS
   (setq bdirp (getpoint "Pick point to show beam direction: "))
   (if (= (getvar "orthomode") 1) ;MAKE bdirp ORTHO W/ RESPECT TO bip IF ORTHOMODE IS ON		
       (if (< (abs (- (car bip ) (car bdirp))) (abs (- (cadr bip ) (cadr bdirp))))
           (setq bdirp (list (car bip) (cadr bdirp)))
           (setq bdirp (list (car bdirp) (cadr bip)))
       )
   )
   (setq wbip (trans bip 1 0))    ;GENERATE WCS POINT AT bip
   (setq wbdirp (trans bdirp 1 0)) ;GENERATE WCS POINT AT bdirp
   (if (= (tblsearch "ucs" "old") nil) ;SAVE STARTING UCS BEFORE CHANGING
       (command "ucs" "S" "OLD")
       (progn
          (command "ucs" "D" "OLD")
          (command "ucs" "S" "OLD")
       )
   )
   (command "ucs" 3 bip bdirp "") ;SET UCS ORIGIN TO bip & ROTATE UCS TO bdirp
   
   (setq ang (angle wbip wbdirp)) ;MEASURE ANGLE IN WCS
   
   (setq bip (trans wbip 0 1))     ;CONVERT bip TO CURRENT UCS
   (setq bdirp (trans wbdirp 0 1))
                  ;INITIALIZE SPACING LIST 
   (setq spal '())
    
                  ;SUPPORT INSERTION POINTS
   (setq n 0)
   (setq lf 1)    ;	SET LENGTH FLAG TO 
                  ;INITIALIZE SUPPORT IP LIST & SUPPORT HEIGHT LIST
   
   (setq ipht1 (- (nth n cgsht) (* ntenlay 0.25)))     
   (setq sipl (list (list (+ (car bip) (nth n dendata)) (- (cadr bip)(/ (- beamwidth 6.0) 2)))))
   (setq shl (list ipht1)) 
                
   (while (< n (1- (length cgsdata)))
                  
                  ;DETERMINE DRAPE
       (setq drape (abs (- (nth n cgsht) (nth (1+ n) cgsht))))
      
                  ;DETERMINE NO. OF SPACES & SPACING
       (setq d (nth n distdata))
       (setq ns (/ d maxspa))
       (if (> (abs (- ns (fix ns))) 0.01) 
           (setq ns (1+ (fix ns)))
           (setq ns (fix ns))      
       )
       (setq spacing (/ d ns))
       (setq spal (cons (list ns spacing) spal))

                   ;DETERMINE SUPPORT IP
       
       (cond ((= (strcase (nth (1+ n) cgsdata)) "L")        
                 (setq l1 d)
                 (setq l2 (nth (1+ n) distdata))
                 (setq l (+ l1 l2))
                 (setq a2 (/ l1 l))    
             )
             ((/= (strcase (nth n cgsdata)) "L")  ;CANTILEVER
                 (setq l (* 2 d))
                 (setq a2 0.5)
                 (setq l1 d)    
             )
       )                                 
       (setq e1 (nth n cgsht))
       (setq e2 (nth (1+ n) cgsht))
       (setq i 0)
       (while (< i (1- ns))
          (setq x (+ (car (car sipl)) spacing))
          (setq y (cadr (car sipl)))
          (setq sipl (cons (list x y) sipl))
          (if (numberp a1)  ;TEST FOR HARP                              
             (if (>= e1 e2)  ;UNHARPED PROFILE                                  
                 (setq h (shtgen1 l (* (1+ i) spacing) a1 a2 e1 e2))
                 (setq h (shtgen2 l (+ l1 (* (1+ i) spacing)) a1 a2 e1 e2)) ; NOTE: e1 IS e2 & e2 IS e3 IN THE FUNCTION
             )
             (if (>= e1 e2)   ;HARPED PROFILE                    
                 (setq h (- e1 (* (- e1 e2)(/ (+ i 1.0) ns))))                  
                 (setq h (+ e1 (* (- e2 e1)(/ (+ i 1.0) ns))))               
             )
          )                     
          (setq sh  (- h (* ntenlay 0.25)))  ;REDUCE SUPPORT HT TO COMPENSATE FOR NO. OF TENDON LAYERS
          (setq shl (cons sh shl))
          (setq i (1+ i))
       )
                   ;ADD IP AT CGS
       (setq x (+ (car (car sipl)) spacing)) 
       (setq y (cadr (car sipl)))           
       (setq sipl (cons (list x y) sipl))
       (setq sh (+ (cadr bip) (- (nth (1+ n) cgsht) (* ntenlay 0.25))))
       (setq shl (cons sh shl))
       (setq n (1+ n))          
   )
                    ;ADD LAST IP
   
   (setq sipl (reverse sipl))
   (setq shl (reverse shl)) 
   (setq spal (reverse spal))            
)
   
(defun dimspa ()
   (if (/= (tblsearch "layer" "beam_sup_text") nil)
      (command "layer" "t" "beam_sup_text" "s" "beam_sup_text" "")
   )
               ;SET DIMENSION STYLE
   (command "dim" "restore" "tixoff" "e")
            ;SET DIMENSION LINE OFFSET
   (setq doff (list (car bip) (+ (cadr bip)(/ beamwidth 2) (/ scale 3 ))))
   (setq wdoff (trans doff 1 0))
   (setq bep (list (+ (car bip) blength)(cadr bip)))
   (setq wbep (trans bep 1 0))

           ;DIMENSION ENDS OF BEAM IF CGS NOT AT ENDS
   (cond ((/= (car dendata) 0.0)
          (setq dipl (list (+ (car dendata) (car bip)) (+ (cadr bip) (/ beamwidth 2)))) 
          (setq wdipl (trans dipl 1 0))
          (setq dipls (list (car bip) (+ (cadr bip) (/ beamwidth 2))))
          (setq wdipls (trans dipls 1 0))
          (command "ucs" "w")
          (command "dim" "aligned" wdipl wdipls wdoff "" "" "e")
          (command "ucs" "p")
         )
          (T (setq dipl (list (car bip)(+ (cadr bip) (/ beamwidth 2)))))
   )  
   (cond ((/= (last dendata) 0.0)
          (setq dipr (list (- (car bep)(last dendata)) (+ (cadr bep) (/ beamwidth 2))))
          (setq wdipr (trans dipr 1 0))
          (setq diprs (list (car bep) (+ (cadr bep) (/ beamwidth 2))))
          (setq wdiprs (trans diprs 1 0))
          (command "ucs" "w")
          (command "dim" "aligned" wdipr wdiprs wdoff "" "" "e")
          (command "ucs" "p")
         )
   )
                     
                    ;CONSOLIDATE SPACING
      
   (setq i 1)
   (setq cspal '())
   (setq ispa (cadr (car spal)))  ;INITIALIZE SPACING 
   (setq nspa (car (car spal)))   ;INITIALIZE NO. OF SPACES
   (while (< i (length spal))
       (if (= (cadr (nth i spal)) ispa)
           (progn
              (setq nspa (+ nspa (car (nth i spal))))
              (setq i (1+ i))
           )
           (progn
              (setq cspal (cons (list nspa ispa) cspal))
              (setq ispa (cadr (nth i spal)))
              (setq nspa (car (nth i spal)))
              (setq i (1+ i))
           )
       )
   )
   (setq cspal (cons (list nspa ispa) cspal))
   (setq spal (reverse cspal))
       
                    ;INITIALIZE DIM IP
   (setq i 0)
   (setq lip dipl)
   (setq rip (list (+ (car dipl) (* (car (car spal)) (cadr (car spal)))) (cadr dipl)))
                ;LOOP TO DIMENSION SPACING
   (command "dim" "restore" "tixon" "e")
                  
   
         
   (while (< i (length spal))
                  ;DETERMINE IF ROUNDING UP, DOWN OR NONE
      (setq ns (car (nth i spal)))
      (setq s (rtos (cadr (nth i spal)) 5))
      (setq d (* (car (nth i spal)) (cadr (nth i spal))))
      (cond ((> (- d (* ns (distof s))) 0.1)
               (setq off "(+)")
            )
            ((> (- (* ns (distof s)) d) 0.1)
             (setq off "(-)")
            )
            (T (setq off ""))
      )
      (setq sdim (strcat (itoa ns) " spa @ " 
                         s "\"" off" = "
                         (rtos d 4)
                 )
      )
      
      
      (setq wlip (trans lip 1 0))
      (setq wrip (trans rip 1 0))
      (command "ucs" "w")
      (if (/= (car (nth i spal)) 1)                   
          (command "dim" "aligned"  wlip wrip wdoff sdim "e")
          (command "dim" "aligned" wlip wrip wdoff "" "e")
      )
      (command "ucs" "p")
      (setq i (1+ i))
      (setq lip rip)
      (if (< i (length spal))
          (setq rip (list (+ (car rip) (* (car (nth i spal)) (cadr (nth i spal)))) (cadr rip)))
      )
   )
)      

(defun drawbeam ()  ;DRAW BEAM, TENDONS & SUPPORTS
   (if (/= (tblsearch "layer" "beam_sup") nil)
      (command "layer" "t" "beam_sup" "s" "beam_sup" "")
   )
   (if (= (car dendata) 0.0)
      (setq i 1)
      (setq i 0)
   )
   (setq sl (- beamwidth 6.0))
   (if (= (cadr dendata) 0.0)
      (setq l (1- (length sipl)))
      (setq l (length sipl))
   )  
   (while (< i l)   ;DRAW TENDON
      (setq p (nth i sipl))
      (setq bp (list (car p) (+ (cadr p) sl))) 
      (command "line" p bp "" )     
      (setq i (1+ i))      
   )  
) 

(defun label ()
               ;SET TEXT WIDTH TO 0.7
   (setvar "filedia" 0)
   (command "style" "ROMANS" "ROMANS,SPECIAL" "" 0.7 "" "" "" "")
   (setq trot (- (/ (* 180 ang) 3.1416)))
   (setq scarl '(("1/8" "~2") ("1/4" "~4") ("3/8" "~6") ("1/2" "~8")
                       ("5/8" "~0") ("3/4" "~w") ("7/8" "~r"))           
   )
   
            ;LOOP TO LABEL SUPPORT HEIGHTS
   (if (= (car dendata) 0.0)
      (setq n 1)
      (setq n 0)
   )
   (if (= (cadr dendata) 0.0)
      (setq l (1- (length sipl)))
      (setq l (length sipl))
   )     
   (while (< n l)
      (setq lab (nth n shl))
      
      ;(setq lab (* 0.125 (fix (/ lab 0.125)))) ROUND TO NEAREST 8TH RATHER THAN DOWN
                     ;INSERT SPECIAL CHARACTERS AT FRACTIONS
      (setq lab (spins lab))     
      (setq labip (list (car (nth n sipl)) 
                        (- (cadr bip) (+ (/ beamwidth 2.0) (/ scale 4)))))
      (if (/= (tblsearch "layer" "beam_sup_text") nil)
         (command "layer" "t" "beam_sup_text" "s" "beam_sup_text" "")
      )
      (command "text" "j" "m" labip trot lab)
      (setq n (1+ n))      
   )
   (setq n 0)
   (setq labip (list (+ (car bip) (car dendata))
                     (- (cadr bip)(* 0.50 scale))))
)

(defun drawtend ()
   (if (/= (tblsearch "layer" "beam_tendon") nil)
      (command "layer" "t" "beam_tendon" "s" "beam_tendon" "")
   )
   (setq ang (angle bip bep)) 
   (if (= (strcase (car cgsdata)) "S")
       (progn
          (setq p1 (list (+ (car bip) (* 16 (cos ang))) (+ (cadr bip) (* 16 (sin ang)))))
          (setq sp bip)
       )
       (progn
          (setq p1 (list (+ (car bip) (* 3 (cos ang))) (+ (cadr bip) (* 3 (sin ang)))))
          (setq sp (list (+ (car bip) (* 2 (cos ang))) (+ (cadr bip) (* 2 (sin ang)))))
       )
   )
   (setq ang (- ang pi))
   (if (= (strcase (car (reverse cgsdata))) "S")
       (progn
          (setq p2 (list (+ (car bep) (* 16 (cos ang ))) (+ (cadr bep) (* 16 (sin ang)))))
          (setq ep bep)
       )
       (progn
          (setq p2 (list (+ (car bep) (* 3 (cos ang))) (+ (cadr bep) (* 3 (sin ang)))))
          (setq ep (list (+ (car bep) (* 2 (cos ang))) (+ (cadr bep) (* 2 (sin ang)))))
       )   
   )
   (command "pline" sp "w" 0 8 p1 "w" 0 0 p2 "w" 8 0 ep "")
)
   
 
(defun spins (c) ;FUNCTION TO INSERT SPECIAL CHARACTERS @ FRACTIONS 
   (setq c (rtos c 5 2))
         (if (/= (strlen c) 1)
            (if (= (substr c (1- (strlen c)) 1) "/")   ;FIND NEXT-TO-LAST DIGIT IN FRACTION
               (if (> (strlen c) 3)
                   (setq c (strcat (substr c 1 (- (strlen c) 4))  ;CONVERT TO SPECIAL FONT IF > 7/8"
                                 (cadr (assoc (substr c (- (strlen c) 2) 3)
                                                scarl))
                           )
                   )
                   (setq c (cadr (assoc c scarl)))     ;CONVERT TO SPECIAL FONT IF <= 7/8"
               )
            )
        )
   (eval c)
)
 
(defun shtgen1 (l x a1 a2 e1 e2 / y)
   (if (and (< (/ x l) a1) (/= a1 0))
      (setq y (- e1 (* (/ (- e1 e2) (* (expt l 2) a1 a2)) (expt x 2))))
      (setq y (+ e2 (* (/ (- e1 e2) (* (expt l 2) a2 (- a2 a1))) (expt (- x (* a2 l)) 2))))
   )     
)
    
(defun shtgen2 (l x a3 a2 e2 e3 / y)     
   (if (and (> x (- l (* l a3))) (/= a1 0))
      (setq y (- e3 (* (/ (- e3 e2) (* (expt l 2) (- 1 a2) a3)) (expt (- x  l) 2))))
      (setq y (+ e2 (* (/ (- e3 e2) (* (expt l 2) (- 1 a2) (- 1 a2 a3))) (expt (- x (* a2 l)) 2))))     
   )     
)






   