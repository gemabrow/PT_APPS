(defun c:chairbom ()
  (setq scalelist '(192.0 128.0 96.0 72.0 48.0 36.0 24.0 16.0 12.0 8.0 1270.0 2540.0 3810.0 5080.0 6350.0 7620.0 8890.0 10160.0 11430.0 12700.0))

  ;PROGRAM TO READ ATTRIBUTE FILE (C:\ACADWIN\CCS\CHAIRBOM.TXT)
  ;AND TO WRITE A CHAIR BILL-OF-MATERIALS BACK TO DRAWING
  (setq osmode (getvar 'osmode))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "FILEDIA" 0)

  ;SET OUTPUT PATH
  (setq current_dir (getvar 'Dwgprefix))
  (setq chairbom_output (strcat current_dir "\chairbom.txt"))
  (setq current_dir (getvar 'Dwgprefix))

  ;EXTRACT ATTRIBUTES
  (command "attext" "" "c:\\apps\\PT_CAD\\bom\\chairtem.txt" chairbom_output)

  (setvar "FILEDIA" 1)
  (setq a (open chairbom_output "r"))
  (setq charlist
    '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "~" "/" "\"" "q" "w" "e" "r" "t"))
  (setq scale (getvar "dimscale"))
  (if (not (member scale scalelist))
    (progn
      (setq scale (getreal "Enter scale factor - 96 for 1/8\", 128 for 3/32\", 192 for 1/16\" [SCALE FACTOR X 25.4 FOR METRIC]: "))
      (setvar "dimscale" scale)))
  (command "style" "ROMANS" "ROMANS.shx,SPECIAL.shx" (/ (getvar "dimscale") 8.0) 0.9 0 "N" "N" "N")
  (setq chrhts '())
  (setq alist '())
  (listatt)
  (close a)
  (setq sp (getpoint "Pick starting point for chair schedule: "))
  (printatt (length slist) 0 sp)
  ;(printctable sp)
  (setvar "CMDECHO" 1)
  (setvar "OSMODE" osmode))

(defun listatt ()
  (while (setq l (read-line a))
    (setq cl (proline l 1 nil nil ""))
    (setq lastc (substr (car cl) (strlen (car cl)) 1))

    ;HANDLE NO QUANTITY CALLOUT IN MULT. CHAIR BLOCK ;;;;;;;
    ;CONSOLIDATE CALLOUTS W/ AND W/O INCH MARKS ;;;;;;;
    (if (/= lastc "\"")
      (progn
        (setq htinch (strcat (car cl) "\""))
        (setq cl (list htinch (cadr cl)))))
 ;IF QUANTITY IS 0 THEN CHAIR CALLOUT QUANTITY IS TWO
    (if (= (cadr cl) 0)
      (setq cl (subst 2 0 cl)))

    (if (not (member (car cl) chrhts))
      (progn
        (setq chrhts (cons (car cl) chrhts))
        (setq alist (cons cl alist)))
      (progn
        (setq old (assoc (car cl) alist))
        (setq new (append old (cdr cl)))
        (setq alist (subst new old alist)))))
  (setq slist '()

;;;				Sort alsit descending order    					;;;
;;;	The cons function for slist reverses the order thus making it ascneding order     	;;;

   (foreach x (vl-sort alist
                '(lambda (a b) (> (_ToDecimal (Car a)) (_ToDecimal (car b)))))
      (setq slist (cons (list (car x) (apply '+ (cdr x))) slist)))))

(defun proline (l cn 2f h n / hnl cc)
  (setq cc (substr l cn 1))
  (cond
    ((= cn (1+ (strlen l))
      (setq hnl (list h (atoi n)))))
    ((and (member cc charlist) (not 2f)
      (if (= h nil)
        (setq h cc)
        (setq h (strcat h cc)))
      (proline l (1+ cn) nil h n)))
    ((= cc ","
      (proline l (1+ cn) T h n)))
    ((and (member cc charlist) 2f
      (if (= n "")
        (setq n cc)
        (setq n (strcat n cc)))
      (proline l (1+ cn) T h n)))
    (T (proline l (1+ cn) 2f h n))))

(defun printatt (n i sp)
  (setq l (car (nth i slist)))
  (command "text" sp 0 l)
  (setq l (itoa (cadr (nth i slist))))
  (setq np (list (+ (car sp) (* scale 0.75)) (cadr sp)))
  (command "text" np 0 l)
  (setq sp (list (car sp) (- (cadr sp) (/ scale 3))))
  (setq i (1+ i))
  (if (< i n) (printatt n i sp)))

(defun printctable (sp)
  ;Prints CTABLE Block with values from listatt result
  (setq output_layer "BAND_SUP_TEXT")
  (setq chaired_layers "BAND_SUP_TEXT,UNIFORM_SUP_TEXT")
  (setq output_block "CTABLE")
  ;(assoc
  ;  0.00 "\""
  ;  0.25 "~4\""
  ;  0.50 "~8\""
  ;  0.75 "~w\"")
  ;(loop for x from 0.75 to 12 by 0.25)
  ;  (do truncate)
  (command "-LAYER" "_ON" chaired_layers
                    "_T" chaired_layers
                    "_S" output_layer "")
  (setq pour_num (getstring "Pour(s)?"))
  (setq floor_num (getstring "Floor(s)?"))
  (command "-INSERT" output_block insertion_point 1 "use X scale factor" 0 pour_num floor_num
    (cdr (assoc   "~w\""slist))
    (cdr (assoc "1\""   slist))
    (cdr (assoc "1~4\"" slist))
    (cdr (assoc "1~8\"" slist))
    (cdr (assoc "1~w\"" slist))
    (cdr (assoc "2\""   slist))
    (cdr (assoc "2~4\"" slist))
    (cdr (assoc "2~8\"" slist))
    (cdr (assoc "2~w\"" slist))
    (cdr (assoc "3\""   slist))
    (cdr (assoc "3~4\"" slist))
    (cdr (assoc "3~8\"" slist))
    (cdr (assoc "3~w\"" slist))
    (cdr (assoc "4\""   slist))
    (cdr (assoc "4~4\"" slist))
    (cdr (assoc "4~8\"" slist))
    (cdr (assoc "4~w\"" slist))
    (cdr (assoc "5\""   slist))
    (cdr (assoc "5~4\"" slist))
    (cdr (assoc "5~8\"" slist))
    (cdr (assoc "5~w\"" slist))
    (cdr (assoc "6\""   slist))
    (cdr (assoc "6~4\"" slist))
    (cdr (assoc "6~8\"" slist))
    (cdr (assoc "6~w\"" slist))
    (cdr (assoc "7\""   slist))
    (cdr (assoc "7~4\"" slist))
    (cdr (assoc "7~8\"" slist))
    (cdr (assoc "7~w\"" slist))
    (cdr (assoc "8\""   slist))
    (cdr (assoc "8~4\"" slist))
    (cdr (assoc "8~8\"" slist))
    (cdr (assoc "8~w\"" slist))
    (cdr (assoc "9\""   slist))
    (cdr (assoc "9~4\"" slist))
    (cdr (assoc "9~8\"" slist))
    (cdr (assoc "9~w\"" slist))
    (cdr (assoc "10\""   slist))
    (cdr (assoc "10~4\"" slist))
    (cdr (assoc "10~8\"" slist))
    (cdr (assoc "10~w\"" slist))
    (cdr (assoc "11\""   slist))
    (cdr (assoc "11~4\"" slist))
    (cdr (assoc "11~8\"" slist))
    (cdr (assoc "11~w\"" slist))
    (cdr (assoc "12\""   slist))
    0 0 0 "")); 3/4" SB, 1" SB, & 1-1/4" SB))

;;;		pBe Helper function to sort alist variable	;;;
;;;								;;;
(defun _ToDecimal (str / p)
  (if (vl-string-position 126 str)
    (distof (vl-some '(lambda (s)))
      (if (Setq p (vl-string-search (Car s) str))
        (vl-string-subst (cadr s) (Car s) str p))
      '(("~1\"" ".625"))
      ("~2\""  ".125")
      ("~3\""  ".1875")
      ("~4\""  ".25")
      ("~5\""  ".3125")
      ("~6\""  ".375")
      ("~7\""  ".4375")
      ("~8\""  ".5")
      ("~9\""  ".5625")
      ("~0\""  ".625")
      ("~q\""  ".6875")
      ("~w\""  ".75")
      ("~r\""  ".875")
      ("~t\""  ".9375"))
    (atoi str)))
