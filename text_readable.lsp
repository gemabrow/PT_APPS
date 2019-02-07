(defun c:Read (/ pi/2 3pi/2 ss i obj ang)
  ;; make text Readable
  ;; Required subroutines: AT:BoundingBoxMidPoint
  ;; Alan J. Thompson, 12.17.11
  (if (setq pi/2  (/ pi 2.)
            3pi/2 (* pi 1.5)
            ss    (ssget "_:L" '((0 . "MTEXT,TEXT")))
      )
    (repeat (setq i (sslength ss))
      (if (and (> (setq ang (vla-get-rotation
                              (setq obj (vlax-ename->vla-object (ssname ss (setq i (1- i)))))
                            )
                  )
                  pi/2
               )
               (<= ang 3pi/2)
          )
        (vlax-invoke obj 'Rotate (AT:BoundingBoxMidPoint obj) pi)
      )
    )
  )
  (princ)
)

 


(defun AT:BoundingBoxMidPoint (obj / a b)
  ;; Return midpoint between boundingbox of specified VLA-OBJECT
  ;; Alan J. Thompson, 07.13.10
  (if (eq (type obj) 'VLA-OBJECT)
    (progn
      (vla-getboundingbox obj 'a 'b)
      (apply (function (lambda (p1 p2) (mapcar (function (lambda (a b) (/ (+ a b) 2.))) p1 p2)))
             (mapcar (function vlax-safearray->list) (list a b))
      )
    )
  )
)