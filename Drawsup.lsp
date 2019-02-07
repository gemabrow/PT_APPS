(defun c:drawsup ()

  (setq ss_tendon nil)   ;INITIALIZE TENDON SELECTION SET
  (setq nc 0)            ;INITIALIZE NUMBER OF CHAIRS

  (setvar "cmdecho" 0)   ;DO NOT ECHO COMMANDS
  (setvar "attdia" 0)

  (setq scarl '(("1/8" "~2")
                ("1/4" "~4")
                ("3/8" "~6")
                ("1/2" "~8")
                ("5/8" "~0")
                ("3/4" "~w")
                ("7/8" "~r"))
  )

  (setq bsup_exist 'N) ;FLAG FOR EXISTING BEGINNING SUPPORT
  (setq esup_exist 'N) ;FLAG FOR EXISTING ENDING SUPPORT

  (setq maxspa 42) ;Auto-default to 42 until config file is supported.
  ;;GET MAX SUPPORT BAR SPACING FROM CONFIG FILE
  ;;OTHERWISE, DEFAULT TO 42 IF USER PROVIDES NO INPUT
  ;(setq config_filepath (strcat (getvar 'dwgprefix) "\config.txt"))
  ;(if (/= (findfile config_filepath) nil)
  ;  ((setq f (open config_filepath "r"))
  ;   (setq maxspa (read-line f))) ;if file exists, read in config
  ;  ((setq maxspa (getdist "Enter maximum support bar spacing [42\"]: "))
  ;   (if (= maxspa nil)
  ;     (setq maxspa 42)))) ;default to 42 maxspa

  ; Set N value to default of 0.1
  (setq a1 0.1)
  ;(setq a1 "100")
  ;(while (not (or (< (atoi a1) 50)
  ;                (= (strcase a1) "H")))
  ;  (setq a1 (getstring "\nEnter N value (L/N) to locate inflection point,\
  ;                       \ 0 for simple parabola [10]: "))
  ;  (if (= a1  "")
  ;    (setq a1 "10")))
  ;(setq a1 (atof a1))
  ;(if (/= a1 0.0)
  ;  (setq a1 (/ 1.0 a1)))

  ;USER INPUT TO DETERMINE SUPPORT METHOD AT LOWPOINT
  (initget 1 "c C s S")
  (setq lpmethod (getkword "Use [C]hairs or Rebar [S]upports at CGS lower than 1-1/2\": "))

  (initget 1 "u U b B")
  (setq t_layer (getkword "Enter U for Uniform or B for Banded tendon supports: "))
  (if (= (strcase t_layer) "U")
    (setq t_layer "UNIFORM")
    (setq t_layer "BAND"))

  (command "layer" "T" t_layer "")  ;THAW AFFECTED LAYER
  (setq continue_profile "Y")
  (setq hp1 (getreal "Enter first highpoint cgs in inches: "))
  (setq lp (getreal "Enter lowpoint cgs in inches if required, else return: "))
  (setq hp2 (getreal "Enter second highpoint cgs in inches: "))

  (while (/= continue_profile "N")

    ;;DRAW FIRST SUPPORT LINE AT HIGHPOINT (OR GET POINTS IF LINE EXISTS)
    (setq p1 (getpoint "Select both endpoints of support at first highpoint: "))
    (setq p2 (getpoint))
    (terpri)
    (if (= (getvar "orthomode") 1) ;MAKE P2 ORTHO W/ RESPECT TO P1 IF ORTHOMODE IS ON
      (if (< (abs (- (car p1 ) (car p2)))
             (abs (- (cadr p1 ) (cadr p2))))
        (setq p2 (list (car p1) (cadr p2)))
        (setq p2 (list (car p2) (cadr p1)))))

    (if (< (cadr p2)(cadr p1)) ;SORT P1 & P2 W/ RESPECT TO Y
      (progn
        (setq p p1)
        (setq p1 p2)
        (setq p2 p)))

    (setq ss_tendon (ssget "F" (list p1 p2) '((-4 . "<AND")
                                              (-4 . "<OR")
                                              (0 . "POLYLINE")
                                              (0 . "LWPOLYLINE")
                                              (-4 . "OR>")
                                              (-4 . "<OR")
                                              (8 . "UNIFORM")
                                              (8 . "BAND")
                                              (-4 . "OR>")
                                              (-4 . "AND>"))))
    (command "layer" "T" (strcat t_layer "_sup") "")
    (setvar "CLAYER" (strcat t_layer "_sup"))
    (if (ssget "F" (list p1 p2) '((-4 . "<AND")
                                  (-4 . "<OR")
                                  (0 . "POLYLINE")
                                  (0 . "LWPOLYLINE")
                                  (-4 . "OR>")
                                  (-4 . "<OR")
                                  (8 . "UNIFORM_SUP")
                                  (8 . "BAND_SUP")
                                  (-4 . "OR>")
                                  (-4 . "AND>"))) ;DON"T DRAW LINE IF ALREADY EXISTS
      (setq bsup_exist 'Y))

    ;;GET SS OF TENDONS INTERSECTING SUPPORT LINE
    (if (/= ss_tendon nil)
      (setq nc (tend_cnt ss_tendon p1 p2))
      (progn
        (alert "NO TENDONS FOUND ON THE UNIFORM OR BAND LAYERS - PROGRAM CANCELED")
        (exit)))

    ;;DETERMINE CHAIR QUANTITY
    (setq nc (strcat "(" (itoa nc) ")"))
    
    (setq p3 (getpoint "Select both endpoints of support at second highpoint: "))
    (setq p4 (getpoint))
    (terpri)
    (if (= (getvar "orthomode") 1) ;MAKE P4 ORTHO W/ RESPECT TO P3 IF ORTHOMODE IS ON
      (if (< (abs (- (car p3) (car p4))) (abs (- (cadr p3 ) (cadr p4))))
        (setq p4 (list (car p3) (cadr p4)))
        (setq p4 (list (car p4) (cadr p3)))))

    (if (< (cadr p4)(cadr p3)) ;SORT P1 & P2W/ RESPECT TO Y
      (progn
        (setq p p3)
        (setq p3 p4)
        (setq p4 p)))

    (if (ssget "F" (list p3 p4) '((-4 . "<AND")
                                  (-4 . "<OR")
                                  (0 . "POLYLINE")
                                  (0 . "LWPOLYLINE")
                                  (0 . "LINE")
                                  (-4 . "OR>")
                                  (-4 . "<OR")
                                  (8 . "UNIFORM_SUP")
                                  (8 . "BAND_SUP")
                                  (-4 . "OR>")
                                  (-4 . "AND>"))) ;DON'T DRAW LINE IF ALREADY EXISTS
      (setq esup_exist 'Y)
    )

    ;;DETERMINE CORRESPONDING ENDPOINTS
    (if (< (distance p1 p3)(distance p1 p4))
      (progn
        (setq epl1 (list p1 p3))
        (setq epl2 (list p2 p4)))
      (progn
        (setq epl1 (list p1 p4))
        (setq epl2 (list p2 p3))))

    ;;DETERMINE DIM-MODE
    (setq mode "aligned")
    (cond
      ((= (cadr (nth 0 epl1)) (cadr (nth 1 epl1)))
        (setq mode "hor"))
      ((= (car (nth 0 epl1)) (car (nth 1 epl1)))
        (setq mode "vert")))

    ;;DETERMINE END OF GREATEST DISTANCE
    (setq l1 (distance (car epl1) (cadr epl1)))
    (setq l2 (distance (car epl2) (cadr epl2)))
    (if (< l1 l2)
      (setq l l2)
      (setq l l1))

    ;;DETERMINE NO. OF SPACES
    (if (= lp nil)
      (progn
        (setq ns (/ l maxspa))
        (if (> (- ns (fix ns)) 0.01)
          (setq ns (1+ (fix ns)))
          (setq ns (fix ns)))
        (princ ns)

        ;;GENERATE ENDPOINTS OF SUPPORT BARS & DRAW
        (if (= bsup_exist 'N)
          (drawsup nil nil p1 p2 nil nil nil nil nil hp1 'cant t_layer))
        (setq bsup_exist 'N)
        (setq spacing (/ l1 ns)) ;USE l1 TO DETERMINE SPACING TO DETERMINE CHAIR HEIGHTS
        (setq fact (/ 1.0 ns))
        (setq n 1)

        (while (< n ns)   ;CANTILEVER SLAB
          ;AM NOT PASSING HIGH OR LOW POINT
          (drawsup epl1 epl2 nil nil hp1 hp2 spacing n fact nil 'cant t_layer)
          (setq n (1+ n)))

        (if (= esup_exist 'N)
          (drawsup nil nil p3 p4 nil nil nil nil nil hp2 'cant t_layer))
        (setq esup_exist 'N)
        (drawspacingdim ns l1 (nth 0 epl2) (nth 1 epl2) mode t_layer))

      (progn
        (if (= bsup_exist 'N)
          (drawsup nil nil p1 p2 nil nil nil nil nil hp1 nil t_layer))
        (setq bsup_exist 'N)
        (setq ns (/ (/ l 2.0) maxspa))
        (if (> (- ns (fix ns)) 0.01)
          (setq ns (1+ (fix ns)))
          (setq ns (fix ns)))
        (princ ns)
        (setq fact (/ 1.0 ns))
        (setq spacing (/ (/ l1 2.0) ns)) ;USE l1 TO DETERMINE SPACING TO DETERMINE CHAIR HEIGHTS
        (setq p5 (intpt epl1 0.5 1))   ;ENDPOINTS AT LOWPOINT
        (setq p6 (intpt epl2 0.5 1))
        (setq epl3 (list (car epl1) p5))
        (setq epl4 (list (car epl2) p6))
        (setq n 1)

        ;; SLAB W/ LOWPOINT
        (while (< n ns)
          ;AM NOT PASSING HIGH OR LOW POINT
          (drawsup epl3 epl4 nil nil hp1 lp spacing n fact nil nil t_layer)
          (setq n (1+ n)))

        (drawsup nil nil p5 p6 nil nil nil nil nil lp nil t_layer)  ;LABEL LOWPOINT
        (setq epl5 (list p5 (cadr epl1)))
        (setq epl6 (list p6 (cadr epl2)))
        (setq n 1)

        ;; SLAB W/ LOWPOINT
        (while (< n ns)
          ;AM NOT PASSING HIGH OR LOW POINT
          (drawsup epl5 epl6 nil nil lp hp2 spacing n fact nil nil t_layer)
          (setq n (1+ n)))

        (if (= esup_exist 'N)
          (drawsup nil nil p3 p4 nil nil nil nil nil hp2 nil t_layer))
        (setq esup_exist 'N)
        (drawspacingdim (* 2 ns) l1 (nth 0 epl2) (nth 1 epl2) mode t_layer)))

    (progn
      (initget "Y y N n")
      (setq continue_profile (getkword "Continue profile? [Y or N] ")))) ;END WHILE
  (setvar "attdia" 1))

(defun drawspacingdim (ns d p1 p2 mode layer)
  (setvar "CLAYER" (strcat layer "_sup_text"))
  (setq s (rtos (/ d ns) 4 3))
  (cond
    ((> (- d (* ns (distof s 4))) 0.1)
      (setq off "(+)"))
    ((> (- (* ns (distof s)) d) 0.1)
     (setq off "(-)"))
    (T (setq off "")))
  (setq sdim (strcat (itoa ns) " SPA @ " s off))
  (setq dimloc (list (car p1) (+ (cadr p1) 60.0)))
  (if (/= (cadr p1) (cadr p2))
    (setq dimloc (list (+ (car p1) 60.0) (cadr p1))))
  (command "dim" mode p2 p1 dimloc sdim "e"))

(defun drawsup (pl1 pl2 pt1 pt2 h1 h2 s n f fh c lname / ncl)    ;nc - GLOBAL VARIABLE
  (if (not fh)   ;COMPUTE SUPPORT HEIGHT AT INTERMEDIATE SUPPORTS
    (progn
      (setq barep1 (intpt pl1 f n))
      (setq barep2 (intpt pl2 f n))
      (setq xd (* n s))
      (if c
        (setq span (* 2.0 l1))
        (setq span l1))
      (if (>= h1 h2)
        (setq h (shtgen1 span xd a1 0.5 h1 h2))   ;USE L1 AS SPACING IS BASED ON L1
        (setq h (shtgen2 span (+ (/ span 2.0) xd) a1 0.5 h1 h2)))) ; NOTE: e1 IS e2 & e2 IS e3 IN THE FUNCTION
    (progn     ;SUPPORT HEIGHT IS AT HIGH OR LOW POINT
      (setq barep1 pt1)
      (setq barep2 pt2)
      (setq h fh)))

  (setq d (/ (distance barep1 barep2) 2)) ;DISTANCE TO FIND MIDPOINT OF SUPPORT
  (setq a (angle barep1 barep2)) ;ANGLE OF SUPPORT LINE RELATIVE TO X AXIS IN RADIANS
  (setq ad (* (/ a pi) 180))  ;ANGLE IN DEGREES
  (if (and (< ad 270) (> ad 90))  ;DETERMINE BLOCK ROTATION ANGLE
    (progn
      (setq bad (+ 180 ad))    ;ROTATION ANGLE FOR CHAIR BLOCK INSERTION
      (setq baroff (- a (/ pi 4.0)))) ;ANGLE FOR BLOCK OFFSET IN RADIANS TO COMPUTE IP

    (progn
      (setq bad ad)
      (setq baroff (+ a (/ pi 4.0)))))
  (setq ip (list (+ (car barep1) (* d (cos a)) (* (/ 72.0 32)(cos baroff)))
    (+ (cadr barep1) (* d (sin a))(* (/ 72.0 32)(sin baroff)))))

  (if (= t_layer "UNIFORM")
    (command "line" barep1 barep2 "")  ;DRAW UNIFORM SUPPORTS
    (progn    ;DRAW BAND SUPPORTS
      (setq dx2 (* d 2.0))
      (setq bn nil)
      (setq bip (list (+ (car barep1)(* d (cos a)))(+ (cadr barep1) (* d (sin a)))))
      (cond
        ((<= dx2 24.0)
          (setq nc "\ ")
          (setq bn "C:/Apps/PT_CAD/REBAR/BNDSUP24.dwg"))
        ((and (> dx2 24.0) (<= dx2 36.0))
          (setq nc "(3)")
          (setq bn "C:/Apps/PT_CAD/REBAR/BNDSUP36.dwg"))
        ((and (> dx2 36.0) (<= dx2 48.0))
          (setq nc "(3)")
          (setq bn "C:/Apps/PT_CAD/REBAR/BNDSUP48.dwg"))
        ((and (> dx2 48.0) (<= dx2 60.0))
          (setq nc "(3)")
          (setq bn "C:/Apps/PT_CAD/REBAR/BNDSUP60.dwg"))
        ((and (> dx2 60.0) (<= dx2 72.0))
          (setq nc "(4)")
          (setq bn "C:/Apps/PT_CAD/REBAR/BNDSUP72.dwg"))
        ((and (> dx2 72.0) (<= dx2 84.0))
          (setq nc "(4)")
          (setq bn "C:/Apps/PT_CAD/REBAR/BNDSUP84.dwg"))
        ((and (> dx2 84.0) (<= dx2 96.0))
          (setq nc "(5)")
          (setq bn "C:/Apps/PT_CAD/REBAR/BNDSUP96.dwg"))
        ((and (> dx2 96.0) (<= dx2 108.0))
          (setq nc "(5)")
          (setq bn "C:/Apps/PT_CAD/REBAR/BNDSUP108.dwg"))
        ((and (> dx2 108.0) (<= dx2 120.0))
          (setq nc "(5)")
          (setq bn "C:/Apps/PT_CAD/REBAR/BNDSUP120.dwg"))
        ((and (> dx2 120.0) (<= dx2 132.0))
          (setq nc "(6)")
          (setq bn "C:/Apps/PT_CAD/REBAR/BNDSUP132.dwg"))
        ((and (> dx2 132.0) (<= dx2 144.0))
          (setq nc "(6)")
          (setq bn "C:/Apps/PT_CAD/REBAR/BNDSUP144.dwg")))
      (if (/= bn nil)
        (command "insert" bn bip "" ""  bad)
        (progn
          (command "line" barep1 barep2 "")
          (setq nc "(999999)")))))
  ;GET SUPPORT HEIGHT AND CHAIR QUANTITY
  (command "layer" "T" (strcat t_layer "_sup_text") "")
  (setvar "CLAYER" (strcat lname "_sup_text"))
  (if (< h 1.5)   ;BEGIN * DETAIL AT 1.875 CGS (1-1/4" IS SHORTEST NON-STAR DETAIL CHAIR)
    (progn
      (if (and (= (strcase lpmethod) "C") (>= h 1.0))
        (progn
          (setq ch (- h 0.25))  ;COMPUTE CHAIR HEIGHT FOR SINGLE-STAR DETAIL
          (setq ncl "SB")
          (command "insert" "C:/apps/PT_CAD/pt_supt/chair" ip 72.0 ""  bad ncl (strcat (spins ch) "\"")))
        (progn
          (setq lab (strcat (spins h) " CGS **"))  ;LABEL LOWPOINT WHEN SUPPORTING W/O CHAIR
          (command "text" "j" "c" ip bad lab))))
    (progn
      (setq ch (- h 0.75))  ;COMPUTE CHAIR HEIGHT
      (command "insert" "C:/apps/PT_CAD/pt_supt/chair" ip 72.0 ""  bad nc (strcat (spins ch) "\""))))
  (command "layer" "T" (strcat t_layer "_sup") "")
  (setvar "CLAYER" (strcat lname "_sup"))
)

(defun intpt (l fac i / x y)
  (setq x (+ (car (car l))  (* (* (- (car (cadr l))(car (car l))) fac) i)))
  (setq y (+ (cadr (car l)) (* (* (- (cadr (cadr l))(cadr (car l))) fac) i)))
  (list x y))

(defun shtgen1 (l x a1 a2 e1 e2 / y)
  (if (and (< (/ x l) a1) (/= a1 0))
    (setq y (- e1 (* (/ (- e1 e2) (* (expt l 2) a1 a2)) (expt x 2))))
    (setq y (+ e2 (* (/ (- e1 e2) (* (expt l 2) a2 (- a2 a1))) (expt (- x (* a2 l)) 2))))))

(defun shtgen2 (l x a3 a2 e2 e3 / y)
  (if (and (> x (- l (* l a3))) (/= a1 0))
    (setq y (- e3 (* (/ (- e3 e2) (* (expt l 2) (- 1 a2) a3)) (expt (- x  l) 2))))
    (setq y (+ e2 (* (/ (- e3 e2) (* (expt l 2) (- 1 a2) (- 1 a2 a3))) (expt (- x (* a2 l)) 2))))))

;;;FUNCTION TO INSERT SPECIAL CHARACTERS @ FRACTIONS
(defun spins (c)
  (setq c (rtos c 5 2))
  (if (/= (strlen c) 1)
    (if (= (substr c (1- (strlen c)) 1) "/")   ;FIND NEXT-TO-LAST DIGIT IN FRACTION
      (if (> (strlen c) 3)
        ;CONVERT TO SPECIAL FONT IF > 7/8"
        (setq c (strcat (substr c 1 (- (strlen c) 4))
                        (cadr (assoc (substr c (- (strlen c) 2) 3) scarl))))
        ;CONVERT TO SPECIAL FONT IF <= 7/8"
        (setq c (cadr (assoc c scarl))))))
  (eval c))

;;;FUNCTION TO LIST ALL VERTICES POINTS TO DETERMINE CHAIR QUANT.
(defun tend_cnt (ss pt1 pt2 / pl en cl)
  (setq apl '())   ;INITIALIZE LIST OF POINT LISTS (ONE POINT LIST PER SS ENTITY)
  (setq n 0)
  (setq ne (sslength ss)) ; NUMBER OF ENTITIES IN SELECTION SET
  (while (< n ne)
    (setq pl '())  ;INITIALIZE POINT LIST
    (setq en (cdr (car (entget (ssname ss n)))))  ;GET ENTITY NAME OF FIRST ENTITY IN SELECTION SET
    (if (= (cdr (assoc 0 (entget en))) "POLYLINE")   ;DETERMINE IF POLYLINE OR LWPOLYLINE
      (progn
        (setq en (cdr (car (entget (entnext en)))))
        (while (/= (cdr (assoc 0 (entget en))) "SEQEND")  ;BUILD LIST OF VERTICES AT POLYLINE
          (setq pl (cons (cdr (assoc 10 (entget en))) pl))  ; GET POINT AND ADD TO LIST
          (setq en (cdr (car (entget (entnext en)))))))   ; GET ENTITY NAME
      (progn
        (setq cl (entget en))                ;BUILD LIST OF VERTICES AT LWPOLYLINE
        (setq i 0)
        (while (< i (length cl))
          (if (= (car (nth i cl)) 10)
            (setq pl (cons (cdr (nth i cl)) pl )))
          (setq i (1+ i)))))
    (setq apl (cons pl apl))
    (setq n (1+ n)))

  ;FIND INTERSECTION POINT BETWEEN END POINTS OF SUPPORT BAR AND 2 NEAREST NODES IN TENDON POLYLINE
  (setq int_ptl '())
  (setq ln (length apl))
  (setq i 0)
  (while (< i ln)   ;ITERATE THRU LIST OF POINT LISTS
    (setq min_dist 10000.0)
    (setq int_pt nil)
    (setq ptlist (nth i apl))
    (setq lpl (length ptlist))
    (setq n 0)
    (while (< n (1- lpl))   ;ITERATE THRU POINT LIST, STOPPING 1 SHORT OF END
      (setq base_pt (nth n ptlist))
      (setq k (1+ n))
      (while (< k lpl)
        (setq comp_pt (nth k ptlist))
        (if (setq test_pt (inters pt1 pt2 base_pt comp_pt))
          (progn
            (setq test_dist (distance base_pt comp_pt))
            (if (< test_dist min_dist)
              (progn
                (setq int_pt test_pt)
                (setq min_dist test_dist)))))
        (setq k (1+ k)))
      (setq n (1+ n)))
    (setq int_ptl (cons int_pt int_ptl))
    (setq i (1+ i)))

  ;(count int_ptl (cdr int_ptl) (car int_ptl) 0)
  (nocoin int_ptl))

;;;FUNCTION TO COUNT NONCOINCIDENT TENDONS
(defun count (pl cl tp n) ;POINT LIST, COMPARISON LIST, TEST POINT, POINT COUNTER - HOLD TP CONSTANT
  (cond
    ((= (length pl) 2)  ;PROCESS LAST 2 POINTS IN PL
      (if  (< (distance tp (car cl)) 2.0)
        (1+ n)  ;ADD 1 IF LAST 2 POINTS ARE COINCIDENT
        (+ n 2))) ;ADD 2 IF LAST 2 POINTS ARE SEPARATE
    ((= (length cl) 1)    ;INCREMENT N IF LAST POINT IN COMPARISON LIST IS NONCOINCIDENT
      (if (< (distance tp (car cl)) 2.0)
        (count (cdr pl)(cddr pl)(cadr pl) n)
        (count (cdr pl)(cddr pl)(cadr pl) (1+ n))))
    ((< (distance tp (car cl)) 2.0)    ;IF TEST POINT (FIRST POINT IN PL) IS COINCIDENT W/ (car CL)
      (count (cdr pl)(cddr pl)(cadr pl) n))    ;MAKE RECURSIVE CALL TO DELETE FIRST POINT

    (T (count pl (cdr cl) tp n))))  ;MAKE RECURSIVE CALL TO COMPARE TEST POINT TO NEXT POINT IN COMPARISON LIST

;;;COUNT NONCOINCIDENTAL POINTS
(defun nocoin (pl / rl n lo li i n)
  (setq rl '())
  (setq lo (1- (length pl))) ;UPPER LIMIT AT OUTER LOOP
  (setq li (length pl))      ;UPPER LIMIT AT INNER LOOP
  (setq i 0)
  (while (< i lo)          ;OUTER LOOP TO INCREMENT COMPARISON POINT
    (setq cp (nth i pl))
    (setq n (1+ i))
    (setq f 'off)
    (while (< n li)         ;INNER LOOP TO ITERATE THRU REMAINDER OF LIST
      (if (< (distance cp (nth n pl)) 6.0)
        (progn
          (setq n (1+ li))
          (setq f 'on))
        (setq n (1+ n))))
    (if (= f 'off)
      (setq rl (cons cp rl)))
    (setq i (1+ i)))
  (setq rl (cons (car (reverse pl)) rl))  ;ADD LAST MEMBER OF PL TO RL
  (length rl))
