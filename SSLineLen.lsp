;select lines by fixed length or within range
(defun c:SSLineLen ( / *Error* cnt ent fixlen len stpt enpt maxlen minlen
mode ss)
(defun *Error* (Msg)
(cond
((or (not Msg)
(member Msg '("console break"
"Function cancelled"
"quit / exit abort"
)
) ;close member
) ;close or
) ;close condition, no message to display
((princ (strcat "\nError: " Msg))) ;else display message
) ;close cond
(princ)
) ;close defun *Error*

(initget 1 "R F ")
(setq MODE
(getkword "Select lines within (R)ange or (F)ixed length : "))
(if (or (= MODE "") (= MODE "F"))
(setq MODE "F")
) ;end if

(if (= MODE "F")
(setq FIXLEN (getdist "\nEnter fixed line length: "))
(progn
(setq MINLEN (getdist "\nEnter minimum length: "))
(setq MAXLEN (getdist "\nEnter maximum length: "))
) ;end progn else
) ;end if

(if (ssget "I")
(setq SS (ssget "I" (list (cons 0 "PLINE"))))
(setq SS (ssget (list (cons 0 "PLINE"))))) ;end if
(setq CNT 0)
(repeat (sslength SS)
(setq ENT (ssname SS CNT)
STPT (cdr (assoc 10 (entget ENT)))
ENPT (cdr (assoc 11 (entget ENT)))
LEN (distance STPT ENPT)
) ;end setq

(cond
((= MODE "F")
(if (equal FIXLEN LEN 0.00001)
(setq CNT (1+ CNT)) ;then next
(ssdel ENT SS) ;else delete
) ;end if
) ;end cond F

((= MODE "R")
(if
(or
(and (<= MINLEN LEN) (>= MAXLEN LEN))
(equal MINLEN LEN 0.00001)
(equal MAXLEN LEN 0.00001)
) ;end or
(setq CNT (1+ CNT))
(ssdel ENT SS)
) ;end if
) ;end cond R
) ;end conditions
) ;end repeat

(if (> (sslength SS) 0)
(progn
(princ (strcat "Number of objects selected = "(itoa (sslength SS))))
(sssetfirst nil SS)
) ;progn
(princ "No objects met the criteria ") ;else
) ;end if
(*Error* nil)
(princ)
) ;end defun