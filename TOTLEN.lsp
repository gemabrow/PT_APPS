(defun c:totlen ()
   (setq n 0)
   (setq tl 0)
   (setq sl 0)
   (setq shl '())
   (setq ss (ssget)) 
   (while (setq pname (ssname ss n))
      (setq pl (entget pname))
      (if (= (cdr (assoc 0 pl)) "LINE")
         (progn
            (setq p1 (cdr (assoc 10 pl)))
            (setq p2 (cdr (assoc 11 pl)))
            (setq l (distance p1 p2))
            (setq tl (+ tl l))
            (cond ((> l 72)
                   (setq sl (+ sl l)))
                  (T
                     (setq shl (cons l shl))))))
      (setq n (1+ n)))
   (setq n 0) ;SET OUTER COUNTER   
   (setq hl '())  ;INITIALIZE HIT LIST
   (setq cl '())  ;INITIALIZE COMPARE LIST
   (while (< n (length shl))  ;START OUTER LOOP   
      (setq f (fix (nth n shl)))
      (if (not (member f cl))
          (progn
             (setq h 1) ;SET HIT COUNTER
             (setq x n) ;SET INNER COUNTER
             (setq cl (cons f cl))
             (while (< x (length shl)) ;START INNER LOOP
                (if (= f (fix (nth x shl)))
                    (setq h (1+ h))) 
                (setq x (1+ x)))
             (setq hl (cons h hl))))
      (setq n (1+ n)))
   (setq n 0)
   (repeat (1- (length cl))
           (princ "\n NUMBER OF LINES ")(princ (nth n cl))(princ " INCHES LONG = ")(princ (nth n hl))
           (setq n (1+ n)))
   (princ "\n TOTAL LENGTH OF LINES LONGER THAN 6' IN FEET = ")(princ (/ sl 12)) 
   (princ "\n TOTAL LENGTH OF LINES SELECTED IN FEET = ")(/ tl 12))