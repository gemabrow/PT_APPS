;;  BlockReplace.lsp
;;  Two commands: BRS = Block Replace: Selected; BRA = Block Replace: All
;;  Both commands:
;;    1)  Ask User for Block name to use to replace other Blocks with.
;;    2)  Ask again for Block name if it is neither defined in current drawing nor
;;       a drawing in a Support File Search Path.
;;    3)  Remember that Block name [separately for each], and offer as default on
;;       subsequent use.  If no default yet in current command, offer default from
;;       other command if present; if not, regular Insert's default if present.
;;    4)  Retain layer, insertion point, scales, rotation, extrusion direction, any
;;       overrides, etc. of all replaced Block insertions.
;;    5)  Also replace any Minsert objects among selection in BRS or of correct
;;       Block name in BRA.
;;  See further details at head of each command definition.
;;  Kent Cooper, last edited 10 October 2012

(vl-load-com)

(defun brerr (errmsg)
  (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
    (princ (strcat "\nError: " errmsg))
  ); if
  (command "_.undo" "_end")
  (setvar 'cmdecho cmde)
  (princ)
); defun -- brerr

(defun *ev (ltr); evaluate what's in variable name with letter
  (eval (read (strcat "*br" ltr "name")))
); defun - *ev

(defun brsetup (this other / temp)
  (setq cmde (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (command "_.undo" "_begin")
  (while
    (or
      (not temp); none yet [first time through (while) loop]
      (and (not (*ev this)) (not (*ev other)) (= temp (getvar 'insname) ""))
        ; no this-command or other-command or Insert defaults yet, on User Enter
      (and ; availability check
        (/= temp ""); User typed something other than Enter, but
        (not (tblsearch "block" temp)); no such Block in drawing, and
        (not (findfile (strcat temp ".dwg"))); no such drawing in Search paths
      ); and
    ); or
    (setq temp
      (getstring
        (strcat
          (if (and temp (/= temp "")) "\nNo such Block or Drawing available." "")
          "\nBlock to Replace existing Block(s) with"
          (cond
            ((*ev this) (strcat " <" (*ev this) ">")); prior Block in this command, if any
            ((*ev other) (strcat " <" (*ev other) ">")); prior Block in other command, if any
            ((/= (getvar 'insname) "") (strcat " <" (getvar 'insname) ">")); Insert's default, if any
            (""); no default on first use if no this-command or other-command or Insert defaults
          ); cond
          ": "
        ); strcat
      ); getstring & temp
    ); setq
  ); while
  (set (read (strcat "*br" this "name"))
    (cond
      ((/= temp "") temp); User typed something
      ((*ev this)); default for this command, if any
      ((*ev other)); default for other command, if any
      ((getvar 'insname)); Enter on first use with Insert's default
    ); cond
  ); set
  (if (not (tblsearch "block" (*ev this))); external drawing, not yet Block in current drawing
    (command "_.insert" (*ev this) nil); bring in definition, don't finish Inserting
  ); if
); defun -- brsetup


(defun C:BRS ; = Block Replace: Selected
;;  To Replace User-selected Block(s) of any name(s) with User-specified Block name.
;;  [Notice of selected Block(s) on locked Layers is within selection; not listed at end
;;    as in BRA;  off or frozen Layers are irrelevant with User selection.]
;;  Rejects Xrefs, but does replace any Windows Metafile objects among selection.
  (/ *error* cmde ss repl notrepl ent)
  (setq *error* brerr)
  (brsetup "s" "a")
  (prompt (strcat "\nTo replace Block insertion(s) with " *brsname ","))
  (setq
    ss (ssget ":L" '((0 . "INSERT"))); Blocks/Xrefs/Minserts/WMFs on unlocked Layers
    repl (sslength ss) notrepl 0
  ); setq
  (repeat repl
    (setq ent (ssname ss 0))
    (if (not (assoc 1 (tblsearch "block" (cdr (assoc 2 (entget ent)))))); not Xref
      (vla-put-Name (vlax-ename->vla-object ent) *brsname); then
      (setq notrepl (1+ notrepl) repl (1- repl)); else
    ); if
    (ssdel ent ss)
  ); repeat
  (prompt (strcat "\n" (itoa repl) " Block(s) replaced with " *brsname "."))
  (if (> notrepl 0) (prompt (strcat "\n" (itoa notrepl) " Xref(s) not replaced.")))
  (command "_.undo" "_end")
  (setvar 'cmdecho cmde)
  (princ)
); defun -- BRS


(defun C:BRA ; = Block Replace: All
;;  To Replace All insertions of designated Block name with User-specified different Block.
;;  Designation of Block name to replace by either selection of Block or Typing.
;;  Replaces insertions in all spaces and layouts.
;;  Does not replace those on locked or frozen Layers [does on Layers that are off, but
;;    see comments below to not replace those], or nested insertions.
  (/ *error* cmde done esel edata oldname ss repl notrepl ent elay layst)
  (setq *error* brerr)
  (brsetup "a" "s")
  (while (not done)
    (setvar 'errno 0)
    (initget "Type")
    (setq esel
      (entsel
        (strcat
          "\nSelect Block to replace all instances with "
          *braname
          " or [Type-it]: "
        ); strcat
      ); entsel
    ); setq
    (cond
      ( (= esel "Type")
        (while
          (not
            (and
              (setq oldname (getstring "\nBlock name: "))
              (tblsearch "block" oldname); Block definition exists in drawing
            ); and
          ); not
          (prompt "\nBlock name not defined --")
        ); while
        (setq done T)
      ); Type-it condition
      ( (= (getvar 'errno) 0); picked something
        (if
          (and
            (setq edata (entget (car esel)))
            (wcmatch (cdr (assoc 0 edata)) "INSERT")
            (setq oldname (cdr (assoc 2 edata)))
            (not (assoc 1 (tblsearch "block" oldname))); not Xref
          ); and
          (setq done T); then
          (prompt "\nSelected object not a Block/Minsert/WMF --"); else
        ); if
      ); picked-something condition
      ((prompt "\nNothing selected --")); missed -- 'errno = 7
    ); cond
  ); while
  (setq
    ss (ssget "_X" (list '(0 . "INSERT") (cons 2 oldname))); all such Blocks/Minserts
    repl (sslength ss) notrepl 0
  ); setq
  (repeat repl
    (setq
      ent (ssname ss 0)
      elay (tblsearch "layer" (cdr (assoc 8 (entget ent))))
      layst (cdr (assoc 70 elay)); contains frozen/locked status
    ); setq
    (if (or (= (logand layst 1) 1) (= (logand layst 4) 4)); on locked [1] or frozen [4] Layer
;; to have it NOT replace those on Layers that are turned OFF, replace above line with following lines:
;;    (if (or (= (logand layst 1) 1) (= (logand layst 4) 4) (minusp (cdr (assoc 62 elay))))
;;      ; on locked [1] or frozen [4] or off [negative color number] Layer
      (setq notrepl (1+ notrepl) repl (1- repl)); then
      (vla-put-Name (vlax-ename->vla-object ent) *braname); else
    ); if
    (ssdel ent ss)
  ); repeat
  (prompt (strcat "\n" (itoa repl) " " oldname " Block(s) replaced with " *braname "."))
  (if (> notrepl 0) (prompt (strcat "\n" (itoa notrepl) " on locked/frozen Layer(s) not replaced.")))
;; if NOT replacing those on Layers that are turned OFF, replace above line with following line:
;;  (if (> notrepl 0) (prompt (strcat "\n" (itoa notrepl) " on off/locked/frozen Layer(s) not replaced.")))
  (command "_.undo" "_end")
  (setvar 'cmdecho cmde)
  (princ)
); defun -- BRA

(prompt "\nBlock Replace: BRS = Select insertion(s), BRA = All insertions of selected Block.")