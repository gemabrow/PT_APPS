; C:RCLOUD draws a revision cloud using sketch. 
;It assumes the last entity drawn is the polyline you want edited.

(defun C:RCLOUD ( / head hdata bulge en ed)
  (rcloud1)
  (setq head (entlast)     ;pline head entity
        hdata (entget head);head data
 ); setq
 (if (= (dxf 70 hdata) 0)    ;test to see if it's a closed entity
 (command "pedit" head "C" "") ;close it
 ); setq
 (setq bulge (list (cons 42 0.5))) ;make the bulge association list
 (setq en (dxf -1 hdata))  ;get first subentity - vertex
 (while (setq en (entnext en)) ; loop through each vertex
 (setq ed (entget en)) ;get vertex data
 (setq ed (append ed bulge)) ;tack on bulge association list
 (entmod ed) ; modify the subentity
 ) ; while    ; update the entire polyline
 (entupd head) ; update the entire polyline
 ); defun
 (princ)

(defun dxf (code elist)
   (cdr (assoc code elist))
);defun

(defun RCLOUD1 ()

   (setq startbox (getpoint "\nPick lower left corner of rectangle: "))
   (setq endbox (getcorner startbox "\nPick upper right corner of rectangle: "))
   (setq x0 (car startbox) ; x value for lower left corner
         y0 (cadr startbox) ; y value for lower left corner
         xf (car endbox) ; x value for upper right corner
         yf (cadr endbox) ; y value for upper right corner
         Rl (- xf x0) ; Rectangle length
         Rh (- yf y0) ; Rectangle height
         p1 (list (+ x0 (/ Rl 6)) (- y0 (/ Rh 6)))
         p2 (list (+ x0 (/ Rl 3)) (- y0 (/ Rh 4)))
         p3 (list (+ x0 (/ Rl 2)) (- y0 (/ Rh 4)))
         p4 (list (+ x0 (/ (* 2 Rl) 3)) (- y0 (/ Rh 4)))
         p5 (list (+ x0 (/ (* 5 Rl) 6)) (- y0 (/ Rh 6)))
         p6 (list (+ x0 Rl) y0)
         p7 (list (+ x0 (/ (* 13 Rl) 12)) (+ y0 (/ Rh 3)))
         p8 (list (+ x0 (/ (* 13 Rl) 12)) (+ y0 (* 2 (/ Rh 3))))

         p9(list (- xf (/ Rl 6)) (+ yf (/ Rh 6)))
         p10 (list (- xf (/ Rl 3)) (+ yf (/ Rh 4)))
         p11 (list (- xf (/ Rl 2)) (+ yf (/ Rh 4)))
         p12 (list (- xf (/ (* 2 Rl) 3)) (+ yf (/ Rh 4)))
         p13 (list (- xf (/ (* 5 Rl) 6)) (+ yf (/ Rh 6)))
         p14 (list (- xf Rl) yf)
         p15 (list (- xf (/ (* 13 Rl) 12)) (- yf (/ Rh 3)))
         p16 (list (- xf (/ (* 13 Rl) 12)) (- yf (* 2 (/ Rh 3))))

    );setq
   (command "pline" startbox p1 p2 p3 p4 p5 p6 p7 p8 endbox p9 p10 p11 p12 p13 p14 p15 p16 "C")
);defun
