(defun c:beam_spt ()

   
  
                   ;SET MAXIMUM SPACING
                     ;(setq maxspa 48)  
                     (setq scale (getvar "dimscale"))
                     (setvar "cmdecho" 0)
                     (setq omo (getvar "osmode")) ;SAVE CURRENT OSMODE SETTINGS
    (if (= (cdr (assoc '40 (tblsearch "style" (getvar "textstyle")))) 0.0) ;RESET TEXT HT TO 1/8" IF CURRENT HT IS 0"
       (command "style" "ROMANS" "ROMANS.shx,SPECIAL.shx" (/ (getvar "dimscale") 8.0) 0.9 0 "N" "N" "N")  
    )
    (input)
    (genip)          
    (drawbeam)
    (label)
    (dimspa)
    (command "style" "DIMTEXT" "ROMANS" "" 0.7 "" "" "" "")
    (setvar "filedia" 1)
    (if (/= omo nil)
       (setvar "osmode" omo)  ;RESTORE CURRENT OSMODE SETTINGS
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
   (setq beamdepth (getreal "Enter beam depth in inches: "))
   (setq ncgs 1)

   (setq nspan (getint "Enter total number of complete spans in beam: "))
   (initget "L l R r B b")
   (setq cant (getkword "Enter L[left], R[right] or B[both] for location of cantilever or return if none: "))
   (if (not cant) (setq cant "N"))
   (initget "Y y N n")
   (setq lowlocf (getkword "Are all CGS lowpoints at midpoint of span?[Y or N] :"))
   (setq lowl nil)    
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
   (setq bip (getpoint "Pick lower left beam corner: "))
   (setq blr (list (+ (car bip) blength) (cadr bip)))
   (setq bur (list (+ (car bip) blength) (+ (cadr bip) beamdepth)))
   (setq bul (list (car bip) (+ (cadr bip) beamdepth)))
                  ;INITIALIZE SPACING LIST 
   (setq spal '())
    
                  ;TENDON INSERTION POINTS
   (setq n 0)
                  ;INITIALIZE TENDON IP LIST
   (setq ipht1 (- (nth n cgsht) (* ntenlay 0.25)))     
   (setq tipl (list (list (car bip) (+ (cadr bip) ipht1))))
   
                ;SECOND TENDON INSERTION POINT
   (if (/= (car dendata) 0.0)
      (setq tipl (cons (list (+ (car bip) (nth n dendata))(cadr (car tipl))) tipl))
   ) 
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

                   ;DETERMINE TENDON IP
      
       (setq lowpoint (cadr bip))
       ;a1 - INFLECTION POINT LOCATION   a2 - LOWPOINT LOCATION AS % OF SPAN
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
          (if (numberp a1)  ;TEST FOR HARP                              
             (if (>= e1 e2) ;UNHARPED PROFILE                                    
                 (setq h (shtgen1 l (* (1+ i) spacing) a1 a2 e1 e2))
                 (setq h (shtgen2 l (+ l1 (* (1+ i) spacing)) a1 a2 e1 e2)) ; NOTE: e1 IS e2 & e2 IS e3 IN THE FUNCTION
             )
             (if (>= e1 e2)   ;HARPED PROFILE                          
                 (setq h (- e1 (* (- e1 e2)(/ (+ i 1.0) ns))))                                
                 (setq h (+ e1 (* (- e2 e1)(/ (+ i 1.0) ns))))               
             )
          )                     
          (setq x (+ (car (car tipl)) spacing))
          (setq y (- (+ lowpoint h)(* ntenlay 0.25)))
          (setq tipl (cons (list x y) tipl))
          (setq i (1+ i))
       )
                   ;ADD IP AT CGS
       (setq x (+ (car (car tipl)) spacing)) 
       (setq y (+ (cadr bip) (- (nth (1+ n) cgsht) (* ntenlay 0.25))))            
       (setq tipl (cons (list x y) tipl))
       (setq n (1+ n))          
   )
                    ;ADD LAST IP
   (cond ((/= (cadr dendata) 0.0)
          (setq lip (car tipl))
          (setq lip (list (+ (car lip) (car (reverse dendata)))
                         (cadr lip))
          )
          (setq tipl (cons lip tipl))
         )
   )
   (setq tipl (reverse tipl)) 
   (setq spal (reverse spal))            
)
   
(defun dimspa ()
   (command "layer" "t" "beam_text" "s" "beam_text" "")
               ;SET DIMENSION STYLE
   (command "dim" "restore" "tixoff" "e")
            ;SET DIMENSION LINE OFFSET
   (setq doff (list (car bul) (+ (cadr bul) scale)))

           ;DIMENSION ENDS OF BEAM IF CGS NOT AT ENDS
   (cond ((/= (car dendata) 0.0)
          (setq dipl (list (+ (car dendata) (car bul)) (cadr bul) ))
          (command "dim" "hor" dipl bul doff "" "e")
         )
         (T (setq dipl bul))
   )  
   (cond ((/= (last dendata) 0.0)
          (setq dipr (list (- (car bur)(last dendata))(cadr bur)))
          (command "dim" "hor" dipr bur doff "" "e")
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
   (setq rip (list (+ (car dipl) (* (car (nth i spal)) (cadr (nth i spal)))) (cadr dipl)))
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
      (if (/= (car (nth i spal)) 1)                   
          (command "dim" "hor" lip rip doff sdim "e")
          (command "dim" "hor" lip rip doff "" "e")
      )
      (setq i (1+ i))
      (setq lip rip)
      (if (< i (length spal))
          (setq rip (list (+ (car lip) (* (car (nth i spal)) (cadr (nth i spal)))) (cadr dipl)))
      )
   )
)      

(defun drawbeam ()  ;DRAW BEAM, TENDONS & SUPPORTS
   (command "layer" "t" "beam_sup" "s" "beam_sup" "")
   (command "pline" bip "w" 0.0 0.0 blr bur bul "c") ;DRAW BEAM
       ;GENERATE POINTS IN TIPL FOR STRESSEND & DEADEND ANCHORS
   (setq tipl_bak tipl)
   (if (= (strcase (car cgsdata)) "S")
       (setq lapoff 12.0) ;SET LEFT ANCHOR POINT OFFSET
       (progn
          (setq lapoff 1.0)
          (setq tipl (append (list (list (+ (car (car tipl)) 3.0) (cadr (car tipl)))) (cdr tipl)))
       )
   )
   (if (= (strcase (car (reverse cgsdata))) "S")
       (setq rapoff 12.0)   ;SET RIGHT ANCHOR POINT OFFSET
       (progn
          (setq rapoff 1.0)
          (setq tipl (reverse tipl))
          (setq tipl (append (list (list (- (car (car tipl)) 3.0) (cadr (car tipl)))) (cdr tipl)))
          (setq tipl (reverse tipl))
       )
   )
   (setq n 0)
   (setq endoff lapoff)
   (while (< n 2)
      (setq dp (distance (car tipl) (cadr tipl)))                                      
      (setq x1 (car (car tipl))) ;SET COORD. FOR FIRST 2 POINTS IN POLYLINE         
      (setq y1 (cadr (car tipl)))
      (setq x2 (car (cadr tipl)))
      (setq y2 (cadr (cadr tipl)))
      (setq offpx (+ x1 (* (- x2 x1) (/ endoff dp)))) ;GENERATE COORD. FOR INTERMEDIATE POINT TO DRAW DEAD OR STRESS END
      (setq offpy (+ y1 (* (- y2 y1) (/ endoff dp)))) 
      (setq tipl (append (list (car tipl) (list offpx offpy)) (cdr tipl))) ;INSERT INTERMEDIATE POINT IN TENDON LIST
      (setq tipl (reverse tipl))  ;REVERSE LIST TO DO OTHER END OF TENDON
      (setq endoff rapoff)
      (setq n (1+ n))
   )
   (setq i 1)
   (setq sp (car tipl))
   (setq tenset (ssadd))
   (setq bt 0.0)
   (setq et 6.0)
   (while (< i (length tipl))   ;DRAW TENDON
      (setq p (nth i tipl))
      (if (= i (1- (length tipl)))
         (progn         
            (setq bt 6.0)
            (setq et 0.0)
         )
      )
      (setq bp (list (car p) (cadr bip)))
      (command "pline" sp "w" bt et p "")
      (setq tenset (ssadd (ssname (ssget "l") 0) tenset))
      (if (and (> i 1) (< i (- (length tipl) 2)))    ;DRAW SUPPORT
              (command "line" p bp "" )
      )
      (setq sp p)
      (setq i (1+ i))
      (setq tenset (ssadd (ssname (ssget "l") 0) tenset))
      (setq bt 0.0 et 0.0)
   )
   (command "pedit" "l" "j" tenset "" "")
   (command "chprop" "l" "" "layer" "beam_tendon" "") 
   (setq tipl tipl_bak)
   
   (redraw)
) 

(defun label ()
   (command "layer" "t" "beam_sup_text" "s" "beam_sup_text" "")
               ;SET TEXT WIDTH TO 0.7
   (setvar "filedia" 0)
   (command "style" "ROMANS" "ROMANS,SPECIAL" "" 0.7 "" "" "" "")

   (setq n 1)
   (setq scarl '(("1/8" "~2") ("1/4" "~4") ("3/8" "~6") ("1/2" "~8")
                       ("5/8" "~0") ("3/4" "~w") ("7/8" "~r"))           
   )
   (setq beambot (cadr bip))
            ;LOOP TO LABEL SUPPORT HEIGHTS
   (while (< n (1- (length tipl)))
      (setq lab (- (cadr (nth n tipl)) beambot))
      ;(setq lab (* 0.125 (fix (/ lab 0.125)))) ROUND TO NEAREST 8TH RATHER THAN DOWN
                     ;INSERT SPECIAL CHARACTERS AT FRACTIONS
      (setq lab (spins lab))
     
      (setq labip (list (car (nth n tipl)) 
                        (- (cadr bip)(* 0.25 scale))))
      (command "text" "j" "m" labip "" lab)
      (setq n (1+ n))      
   )
   (setq n 0)
   (setq labip (list (+ (car bip) (car dendata))
                     (- (cadr bip)(* 0.50 scale))))
                      ;LOOP TO LABEL CGS'
   (while (< n (length cgsht))
      (setq lab (nth n cgsht))
                     ;INSERT SPECIAL CHARACTERS AT FRACTIONS
      (setq lab (spins lab))
      (setq lab (strcat lab "\" CGS"))
      (command "text" "j" "m" labip "" lab)
      (if (/= (nth n distdata) nil)
          (setq labip (list (+ (nth n distdata) (car labip)) 
                        (- (cadr bip)(* 0.50 scale))))
      )
      (setq n (1+ n))
   )                  
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

    