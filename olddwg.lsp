(defun c:newdwg ()     ;MAIN PROGRAM

      ;dwgtype = drawing type - "gn","bc","pt" or "A" 
      ;dsize = drawing size -  "E" or "D" 
      ;scale = drawing scale - eg 96 for 1/8" = 1'-0"
  
   (setq kill n) ;SWITCH TO EXIT PROGRAM
   (setq exists 'n)
   (setvar "CMDECHO" 0)
   (setq gnlist (list '(A "GN1WAY_S") '(B "GN1WAY_V") '(C "GN2WAY_S") '(D "GN2WAY_V") '(E "GNBARCAB")'(F "GNBND_F") '(G "GNBND_R") '(H "GNSOG_1") '(J "GNSOG_2") '(K "GNWILBAR"))) ;DRAWING TYPES                  
   (input)    
   (if (= kill n)
       (progn
          (tgtname)
          (wbatch)
          (if (/= (open newname "r") nil) 
              (progn (alert "***DRAWING BY THIS NAME ALREADY EXISTS, PROGRAM CANCELLED***")
                     (setq exists 'y)
              ) 
          )
          ;SAVE CURRENT DRAWING IF NOT UNNAMED  
         (while (= exists 'n)
            (if  (/= (getvar "dwgname") "Drawing.dwg")
                (progn 
                   (setvar "filedia" 0)
                   (command "save" "")
                   (setq dwgsave "y")
                   (setvar "filedia" 1)
                )
            )
            
           
              ;EXECUTE DOS BATCH FILE TO COPY PROTOTYPE FROM F: TO C:
            (command "zoom" "a")
            (command "sh" "C:\\newdwg.bat")
            (while (not (findfile newname))) 
            (if (/= dwgtype "gn")
                   (progn
                      (wscript)
                      (setq f (open "c:\\newdwg.flg" "w")) ;OPEN FLAG FILE FOR WRITING
                      (prin1 '1 f)
                      (close f)
                   )     
            )
            ;(if (and (= dwgtype  "gn") (/= (getvar "dwgname") "Drawing.dwg"))
            ;     (while (not (findfile (getvar "dwgname"))))                  
            ;)
            (setq delay 100000)
            (while (> delay 0)
               (setq delay (1- delay))
            )
            (while (not (findfile newname)))   
            (setvar "filedia" 0)
            (command "fileopen" "Y" newname)                   
           
         )
       )
   )
   
)              ;END MAIN PROGRAM


(defun input ()
         ;;;;INITIAL DIALOG BOX
      (setq jobn nil dwgtype nil dwgn nil)
      (setq dcl_id (load_dialog "f:\\apps\\ccs_cad\\dcl\\newdwg.dcl"))
         (while (and (not (and jobn dwgtype dwgn)) (= kill n))
            (if (not (new_dialog "newdwg" dcl_id))
                (exit))
        
                      ;INITIALIZE TILES
                (action_tile "jn" "(setq jobn $value)")
                (action_tile "dt" "(setq dwgtype $value)")
                (action_tile "dn" "(setq dwgn $value)")       
                (action_tile "accept" "(done_dialog)")  
                (action_tile "cancel" "(setq kill $value)(done_dialog)")
   
                      ;RESTORE VALUES TO TILES IN CASE OF ACCEPT W/ NO SELECT
                (if (/= jobn nil) (set_tile "jn" jobn))
                (if (/= dwgtype nil) (set_tile "dt" dwgtype))
                (if (/= dwgn nil) (set_tile "dn" dwgn))

                       ;START DIALOG
                (start_dialog)
                
           )
          
 

      ;;;; DIALOG BOX FOR SCALE
   (if (and (/= dwgtype  "gn") (= kill n))
          (progn (if (not (new_dialog "scale" dcl_id))
                     (exit)
                     (progn
                               (set_tile "s" "96")
                               (setq scal "96")
                               (action_tile "s" "(setq scal $value)")
                               (action_tile "accept" "(done_dialog)")
                               (action_tile "cancel" "(setq kill $value)(done_dialog)")
                               (start_dialog)                                  
                     )
                  )
                   ; DIALOG BOX FOR DRAWING SIZE
                 (if (and (/= dwgtype  "A")(= kill n))
                     (progn (if (not (new_dialog "size" dcl_id))
                                (exit)
                                (progn
                                   (set_tile "ds" "E")
                                   (setq dsize "E") 
                                   (action_tile "ds" "(setq dsize $value)")
                                   (action_tile "accept" "(done_dialog)")
                                   (action_tile "cancel" "(setq kill $value)(done_dialog)")
                                   (start_dialog)                              
                                )
                            )
                           
                     )
                 )
          )        
                 (if (= kill n) ;GENERAL NOTES 
                     (if (not (new_dialog "gntype" dcl_id))
                            (exit)
                         (progn
                           (set_tile "gnt" "A")
                           (setq gtype "A")  
                           (action_tile "gnt" "(setq gtype $value)") ;GET GENERAL NOTE TYPE
                           (action_tile "accept" "(done_dialog)")
                           (action_tile "cancel" "(setq kill $value)(done_dialog)")
                           (start_dialog) 
                           (if (= kill n)
                               (if (not (new_dialog "size" dcl_id))
                                     (exit) 
                                   (progn 
                                      (set_tile "ds" "E")
                                      (setq dsize "E")
                                      (action_tile "ds" "(setq dsize $value)")  ;GET GENERAL NOTE SIZE
                                      (action_tile "accept" "(done_dialog)")
                                      (action_tile "cancel" "(setq kill $value)(done_dialog )")
                                      (start_dialog)
                                   )
                               )
                            )
                         )
                      )
                 )
    ) 
) 

(defun tgtname ()
   (cond ((= dwgtype "pt")
          (setq newname (strcat (substr jobn 1 1) (substr jobn 3) "P" dwgn))
         )
         ((= dwgtype "bc")
          (setq newname (strcat (substr jobn 1 1) (substr jobn 3) "B" dwgn))
         )
         ((= dwgtype "gn")
          (setq newname (strcat (substr jobn 1 1) (substr jobn 3) "G" dwgn))
         )
         ((= dwgtype "A")
          (setq newname (strcat (substr jobn 1 1) (substr jobn 3) "A" dwgn))
         )
   )
)         

(defun wbatch ()
 (setq f (open "c:\\newdwg.bat" "w"))  ;OPEN BATCH FILE FOR WRITING
 (setq l (strcat "IF NOT EXIST F:\\projects\\" jobn " MD F:\\projects\\" jobn)) ;CREATE JOB DIRECTORY IF NOT EXISTING
 (write-line l f)
 (setq newname (strcat "F:\\projects\\" jobn "\\" newname ".DWG")) ;ADD PATH TO FILE NAME
   (cond ((or (= dwgtype "pt")(= dwgtype "bc"))       
           (setq l (strcat "COPY F:\\APPS\\CCS_CAD\\PROTO\\PROTO_" dsize ".DWG "  newname))           
         )
         ((= dwgtype "gn")
           (setq gnproto (strcat "G" dsize (substr (cadr (assoc (read gtype) gnlist)) 3)))       
           (setq l (strcat "COPY F:\\APPS\\CCS_CAD\\PROTO\\" gnproto ".DWG " newname))
         )
         ((= dwgtype "A")
           (setq l (strcat "COPY F:\\APPS\\CCS_CAD\\PROTO\\PROTO_A.DWG " newname))   
         )
   )
 (write-line l f)
 (close f)
) 

(defun wscript ()
  (setq f (open "c:\\newdwg.scr" "w")) ;OPEN SCRIPT FILE FOR WRITING
  (if (/= scal "1")
    (progn
       (setq s (atof scal))
       (cond ((= dwgtype "A")  ;COMPUTE LIMITS
              (setq url (strcat (rtos (* s 8.875) 4 3) "," (rtos (* s 11.375) 4 3)))
             )
             ((= dsize "E")
              (setq url (strcat (rtos (* s 41) 4 3) "," (rtos (* s 29.25) 4 3)))
             )
             ((= dsize "D")
              (setq url (strcat (rtos (* s 33.25) 4 3) "," (rtos (* s 21.375) 4 3)))
             )
       )
       (setq dl '("EXOFF" "TICK8" "TICKEXOFF" "TIXON8" "TIXOFF8" "TIXON" "TIXOFF" "TICK")) ;LIST OF DIM STYLES
       (setq n 0)
       (write-line "dim" f)
       (while (< n (length dl))
           (setq l (strcat "RESTORE " (nth n dl) " DIMSCALE " scal " SAVE " (nth n dl) " Y"))
           (write-line l f)
           (setq n (1+ n))
       )
       (write-line "exit" f)
       (setq l (strcat "SETVAR LTSCALE " (ITOA (/ (ATOI SCAL) 2))))
       (write-line l f)
       (setq l (strcat "SETVAR USERR1 " SCAL))
       (write-line l f)
       (setq l (strcat "SETVAR USERI1 0"))
       (write-line l f)
       (setq l (strcat "SETVAR FILEDIA 0"))
       (write-line l f)
       (setq l (strcat "STYLE DIMTEXT ROMANS " (RTOS (/ (ATOI SCAL) 9.6) 2 2) " 0.9 0.0 N N N"))
       (write-line l f)
       (setq l (strcat "STYLE ROMANS ROMANS,SPECIAL " (RTOS (/ (ATOI SCAL) 9.6) 2 2) " 0.9 0.0 N N N"))
       (write-line l f)
       (setq l (strcat "SETVAR FILEDIA 1"))
       (write-line l f)
       (setq l (strcat "SCALE ALL  0,0 " SCAL))
       (write-line l f)
       (setq l (strcat "LIMITS 0,0 " url))
       (write-line l f)
       (setq l (strcat "ZOOM ALL"))
       (write-line l f)
       (setq l (strcat "TEXT "  scal "," (rtos (* 0.25 (atoi scal)) 2) " 0 "  newname " "))
       (write-line l f)
       (setq l (strcat "SAVE "))
       (write-line l f)
       (close f)
    )
  )
)

