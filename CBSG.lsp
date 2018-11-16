;;  ConcreteBeamSectionGenerator.LSP                ;  Scot Harris  8-15-2017
;;   USER_CBSG is session only gremlin (do not place in the defun area).
;;   Note: All stored values are strings. Where reals and integers are used,
;;   they will have to be converted to such when accessed by the main program.
;;   Edit boxes are updated to the slider values only. Slider values are not
;;   updated to the edit boxes. Only the edit box values will be stored where
;;   edit box/slider combinations.

(defun c:CBSG ( / CBSG-Slab CBSG-Colu CBSG-Reba CBSG-Anch sd dcl_id )

 ;== Function Help dialog ==
 (defun CBSG-Help ( / )
  (alert (strcat
   "\tConcrete Beam Section Generator Help "
   "\n "
   "\nThis is where to provide insight to the users of this program. "
   "\nPlease note that the alert box has a maximum width and when a line is too long, it will spill into the next line. "
  ))
 );end CBSG-Help

 ;== Function gather values and save as a session gremlin list ==
 (defun CBSG_GET ( / )
  (setq USER_CBSG (list
   (get_tile "To11")
   (get_tile "Ed11")
   (get_tile "Po11")
   (get_tile "To21")
   (get_tile "Ra210")
   (get_tile "Ed21")
   (get_tile "Po21")
   (get_tile "Ed22")
   (get_tile "Po22")
   (get_tile "To22")
   (get_tile "Ra310")
   (get_tile "Ed31")
   (get_tile "Ed32")
   (get_tile "Ra320")
   (get_tile "Ed33")
   (get_tile "Ed34")
   (get_tile "Ed35")
   (get_tile "Po31")
  ))
 );end CBSG_GET

 ;== Function edit box increment is 0.5 [Slab Height] ==
 (defun EdiSli-1 ( val / )
  (set_tile "Ed11" (rtos (/ (atoi val) 2.0) 2 1))
 );end EdiSli-1

 ;== Function top of beam width is 4" more than minimum beam width ==
 ;== Adjust top of beam value when beam width becomes less than 4" to ==
 ;== the top of beam value ==
 (defun EdiSli-2 ( val / )
  (set_tile "Ed31" val)
  ;increment up only.
  (if (< (atoi (get_tile "Sl32")) (+ (atoi val) 4))
   (progn
    (set_tile "Ed32" (itoa (+ (atoi val) 4)))
    (set_tile "Sl32" (itoa (+ (atoi val) 4)))
    (if (= (get_tile "Ra310") "Ra311") (mode_tile "Ro32" 1))
   );progn
  );if
 );end EdiSli-2

 ;== Function do not invert trapezoid (switches to Rectangular) ==
 (defun EdiSli-3 ( val / )
  (set_tile "Ed32" val)
  (if (= (get_tile "Ed31") val)
   (progn
    (set_tile "Ra310" "Ra311")
    (mode_tile "Ro32" 1)
    (mode_tile "Ra320" 1)
   );progn
  );if
 );end EdiSli-3

 ;== Function adjust CGS height to 1/2 total beam height when beam ==
 ;== height becomes equal to or less than the current CGS height ==
 (defun EdiSli-4 ( val / )
  (set_tile "Ed33" val)
  ;increment down only.
  (if (>= (atoi (get_tile "Sl34")) (atoi val))
   (progn
    (set_tile "Ed34" (itoa (/ (atoi val) 2)))
    (set_tile "Sl34" (itoa (/ (atoi val) 2)))
   );progn
  );if
 );end EdiSli-4

 ;== Function alert when CGS height exceeds beam height ==
 (defun EdiSli-5 ( val / )
  (set_tile "Ed34" val)
  (if (< (atoi (get_tile "Ed33")) (atoi val))
   (alert "CGS height exceeds beam height.")
  );if
 );end EdiSli-5

 ;== Function turn off trapezoid items when rectangular beam is selected ==
 ;== and adjust top of beam width when turned back on ==
 (defun Mode-1 ( a / )
  (mode_tile "Ro32" a)
  (mode_tile "Ra320" a)
  (if (= a 0)
   (progn
    (set_tile "Ed32" (itoa (+ (atoi (get_tile "Ed31")) 4)))
    (set_tile "Sl32" (itoa (+ (atoi (get_tile "Ed31")) 4)))
   );progn
  );if
 );end Mode-1

 ;==== DIALOG ===============================================================
 (setq sd 0)
 (prompt "Concrete Beam Section Generator ")
 (if (new_dialog "CBSG" (setq dcl_id (load_dialog "CBSG.dcl")))
  (progn
   ;*** INITIALIZE ***
   (or USER_CBSG
    (setq USER_CBSG (list "1" "5.0" "1"
     "1" "Ra211" "24" "1" "3" "0" "1"
     "Ra311" "16" "18" "Ra321" "35" "25" "8" "0")))
   (setq CBSG-Slab (list "  Left" "  Center" "  Right"))
   (setq CBSG-Colu (list "  Left" "  Center" "  Right" "  Offset"))
   (setq CBSG-Reba (list "  #3" "  #4" "  #5" "  #6" "  #7" "  #8" "  #9" " #10" " #11" " #12"))
   (setq CBSG-Anch (list "  Deadend" "  Stressed" "  Intermediate" "  Unspecified"))

   ;*** SET TILE ***
   (set_tile "Tx00" "All measurements are in Inches.")
   ;slab geometry
   (set_tile "To11"  (nth 0 USER_CBSG))
   (set_tile "Ed11"  (nth 1 USER_CBSG))
   (set_tile "Sl11" (itoa (fix (* (distof (nth 1 USER_CBSG) 2) 2)))) ;1/2" increments
   (start_list "Po11")(mapcar 'add_list CBSG-Slab)(end_list)
   (set_tile "Po11"  (nth 2 USER_CBSG))
   ;column geometry
   (set_tile "To21"  (nth 3 USER_CBSG))
   (set_tile "Ra210" (nth 4 USER_CBSG))
   (set_tile "Ed21"  (nth 5 USER_CBSG))
   (set_tile "Sl21"  (nth 5 USER_CBSG))
   (start_list "Po21")(mapcar 'add_list CBSG-Colu)(end_list)
   (set_tile "Po21"  (nth 6 USER_CBSG))
   (set_tile "Ed22"  (nth 7 USER_CBSG))
   (set_tile "Sl22"  (nth 7 USER_CBSG))
   (start_list "Po22")(mapcar 'add_list CBSG-Reba)(end_list)
   (set_tile "Po22"  (nth 8 USER_CBSG))
   (set_tile "To22"  (nth 9 USER_CBSG))
   ;beam geometry
   (set_tile "Ra310" (nth 10 USER_CBSG))
   (set_tile "Ed31"  (nth 11 USER_CBSG))
   (set_tile "Sl31"  (nth 11 USER_CBSG))
   (set_tile "Ed32"  (nth 12 USER_CBSG))
   (set_tile "Sl32"  (nth 12 USER_CBSG))
   (set_tile "Ra320" (nth 13 USER_CBSG))
   (set_tile "Ed33"  (nth 14 USER_CBSG))
   (set_tile "Sl33"  (nth 14 USER_CBSG))
   (set_tile "Ed34"  (nth 15 USER_CBSG))
   (set_tile "Sl34"  (nth 15 USER_CBSG))
   (set_tile "Ed35"  (nth 16 USER_CBSG))
   (set_tile "Sl35"  (nth 16 USER_CBSG))
   (start_list "Po31")(mapcar 'add_list CBSG-Anch)(end_list)
   (set_tile "Po31"  (nth 17 USER_CBSG))

   ;*** MODE TILE ***
   (if (= (get_tile "To11") "0") (mode_tile "Co11" 1))
   (if (= (get_tile "To21") "0") (mode_tile "Co21" 1))
   (if (= (get_tile "Ra310") "Ra311")
    (progn
     (mode_tile "Ro32" 1)
     (mode_tile "Ra320" 1)
    );progn
   );if

   ;*** ACTION TILE ***
   (action_tile "To11" "(mode_tile \"Co11\" (if (= $value \"1\") 0 1))")
   (action_tile "Sl11" "(EdiSli-1 $value)")
   (action_tile "To21" "(mode_tile \"Co21\" (if (= $value \"1\") 0 1))")
   (action_tile "Sl21" "(set_tile \"Ed21\" $value)")
   (action_tile "Sl22" "(set_tile \"Ed22\" $value)")
   (action_tile "Ra311" "(Mode-1 1)")
   (action_tile "Ra312" "(Mode-1 0)")
   (action_tile "Sl31" "(EdiSli-2 $value)")
   (action_tile "Sl32" "(EdiSli-3 $value)")
   (action_tile "Sl33" "(EdiSli-4 $value)")
   (action_tile "Sl34" "(EdiSli-5 $value)")
   (action_tile "Sl35" "(set_tile \"Ed35\" $value)")
   (action_tile "accept" "(CBSG_GET)(done_dialog 1)")
   (action_tile "cancel" "(done_dialog 0)")
   (action_tile "help"   "(CBSG-Help)")
   (action_tile "reset"  "(done_dialog 2)")
   (setq sd (start_dialog))(unload_dialog dcl_id)(setq dcl_id nil)

  );progn
  (alert "Failed to find DCL file or error loading dialog.")
 );if

 ;==== MAIN PROGRAM =========================================================
 (if (= sd 1)
  (progn
   ;*** Execute Main Program Here... ***
   (alert (strcat
"Slab Geometry: "
"\n Show Slab\t"    (nth 0 USER_CBSG)
"\n Slab Height\t"  (nth 1 USER_CBSG)
"\n Slab Justify\t" (nth 2 USER_CBSG)
"\n "
"\nColumn Geometry: "
"\n Show Column\t"    (nth 3 USER_CBSG)
"\n Rect./Circular\t" (nth 4 USER_CBSG)
"\n Column Width\t"   (nth 5 USER_CBSG)
"\n Column Justify\t" (nth 6 USER_CBSG)
"\n Rebar Quantity\t" (nth 7 USER_CBSG)
"\n Rebar Size\t"     (nth 8 USER_CBSG)
"\n Evenly Spaced\t"  (nth 9 USER_CBSG)
"\n "
"\nBeam Geometry: "
"\n Rect./Trapezoid\t"    (nth 10 USER_CBSG)
"\n Beam Width\t"         (nth 11 USER_CBSG)
"\n Top of Beam Width\t"  (nth 12 USER_CBSG)
"\n Width Reference at\t" (nth 13 USER_CBSG)
"\n Beam Height\t"        (nth 14 USER_CBSG)
"\n CGS Height\t"         (nth 15 USER_CBSG)
"\n Number of Anchors\t"  (nth 16 USER_CBSG)
"\n Anchor Type\t"        (nth 17 USER_CBSG)
   ))
  );progn
 );if

 (if (= sd 2)
  (progn
   (setq USER_CBSG nil)
   (alert "Dialog values have been reset.")
  );progn
 );if

 (setq CBSG-Slab nil CBSG-Colu nil CBSG-Reba nil CBSG-Anch nil sd nil)(princ)
);end CBSG

(c:CBSG)
