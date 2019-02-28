(defun c:rot (/ rotateval ss len c e f degrees radians)
  (setq	rotateval
    (atoi (getstring "\nHow many degrees rotation are required: ")))
  (setq ss (ssget))
  (setq len (sslength ss))
  (setq c 0)
  (repeat len
    (setq f (entget (ssname ss c)))
    (setq e (cdr (assoc '50 (entget (ssname ss c)))))
    (setq degrees (Radian->Degrees e))
    (setq degrees (+ degrees rotateval))
    (while (>= degrees 360)
      (setq degrees (- degrees 360)))
    (setq radians (Degrees->Radians degrees))
    (setq f (subst (cons 50 radians) (assoc 50 f) f))
    (entmod f)
    (setq c (+ c 1))))

(defun Radian->Degrees (nbrOfRadians)
  (* 180.0 (/ nbrOfRadians pi)))

(defun Degrees->Radians	(numberOfDegrees)
  (* pi (/ numberOfDegrees 180.0)))