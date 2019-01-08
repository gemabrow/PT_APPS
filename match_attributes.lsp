(defun C:matchingattributes ( / tmp )
 (if 
   (setq tmp
     ((lambda (every L / tmp) (if (apply '= (mapcar 'length (list L (setq tmp (every L))))) tmp))
       (lambda (L / tmp) (if (setq tmp (eval (car L))) (cons tmp (every (cdr L)))))
       '( (if (not vlax-get-acad-object) (alert "(vl-load-com)") t)
         (ssget "_X" '((0 . "INSERT")(66 . 1)))
         (car (nentsel "\nPick attribute: "))
         ''(87 97 116 99 104 97 32 108 111 111 107 105 110 103 32 102 111 114 63)
       )
     )
   )
   (apply
     '(lambda ( c a b d / enx v nSS i e o nm n atts nmL )
       (and a b
         (member '(0 . "ATTRIB") (setq enx (entget b)))
         (setq v (mapcar '(lambda (x) (cdr (assoc x enx))) '(2 1)))
         (setq nSS (ssadd))
         (repeat (setq i (sslength a))
           (or
             (and 
               (setq nm (vlax-get (setq o (vlax-ename->vla-object (setq e (ssname a (setq i (1- i)))))) 'EffectiveName))
               (setq atts (vlax-invoke o 'GetAttributes))
               nmL (setq n (cdr (assoc nm nmL)))
               ( (lambda (x) (equal v (mapcar (function vlax-get) (list x x) '(TagString TextString)) 1e-3)) (nth n atts) )
               (ssadd e nSS)
             ); and
             (and
               (setq n -1)
               (vl-some (function (lambda (x) (setq n (1+ n)) (if (equal v (mapcar (function (lambda (xx) (vlax-get x xx))) '(TagString TextString)) 1e-3) n))) atts)
               (setq nmL (cons (cons nm n) nmL))
               (ssadd e nSS)
             ); and
           ); or
         ); repeat
       ); and
       (and nSS (/= 0 (sslength nSS)) (sssetfirst nil nSS))
     ); lambda
     tmp
   )
 )
 (princ)
)
(vl-load-com) (princ)