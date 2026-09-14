;;; ==================================================================
;;;  R90R.lsp
;;;  ------------------------------------------------------------------
;;;  Commands :
;;;     R90R    - rotate EVERY selected closed polyline by exactly
;;;               +90 degrees around ITS OWN geometric center.
;;;     R90DIAG - diagnostics: tells what is actually in the selection
;;;               (entity types, closed / open, locked layers, blocks),
;;;               i.e. WHY something was not rotated.
;;;
;;;  Rules:
;;;    * one common base point is NEVER used;
;;;    * the center is computed separately for every polyline from its
;;;      own vertices (area centroid, NOT a bounding box), so objects
;;;      rotated at any angle are handled correctly;
;;;    * after rotation each center stays exactly in the same point,
;;;      so distances and relative positions do not change;
;;;    * closed LWPOLYLINE and closed 2D POLYLINE are processed,
;;;      open ones are skipped;
;;;    * a polyline whose last vertex coincides with the first one is
;;;      treated as closed too (very common in real drawings, the
;;;      "Closed" flag is simply not set there).
;;;
;;;  Messages are plain ASCII on purpose, so the file loads correctly
;;;  in any AutoCAD version regardless of file encoding.
;;; ==================================================================

(vl-load-com)

;;; ------------------------------------------------------------------
;;;  helpers
;;; ------------------------------------------------------------------

;; is the layer of the entity locked?
(defun r90r:locked (ed / rec)
  (and (setq rec (tblsearch "LAYER" (cdr (assoc 8 ed))))
       (= 4 (logand 4 (cdr (assoc 70 rec))))
  )
)

;; vertices (DXF 10) of an LWPOLYLINE
(defun r90r:verts-lw (ed / v lst)
  (foreach v ed
    (if (= 10 (car v))
      (setq lst (cons (cdr v) lst))
    )
  )
  (reverse lst)
)

;; VERTEX sub-entities of a heavy 2D POLYLINE -> list of (ename x y z)
(defun r90r:verts-pl (en / v ed lst)
  (setq v (entnext en))
  (while (and v
              (setq ed (entget v))
              (= "VERTEX" (cdr (assoc 0 ed)))
         )
    (if (zerop (logand 16 (cdr (assoc 70 ed))))   ; skip spline frame pts
      (setq lst (cons (cons v (cdr (assoc 10 ed))) lst))
    )
    (setq v (entnext v))
  )
  (reverse lst)
)

;; average of the vertices (fallback for degenerate shapes)
(defun r90r:avg (pts / sx sy n)
  (setq sx 0.0 sy 0.0 n (length pts))
  (foreach p pts
    (setq sx (+ sx (car p))
          sy (+ sy (cadr p))
    )
  )
  (list (/ sx n) (/ sy n))
)

;; true geometric center (area centroid) of a closed polygon:
;;   A  = 1/2      * SUM[ xi*y(i+1) - x(i+1)*yi ]
;;   Cx = 1/(6*A)  * SUM[ (xi + x(i+1)) * (xi*y(i+1) - x(i+1)*yi) ]
;;   Cy = 1/(6*A)  * SUM[ (yi + y(i+1)) * (xi*y(i+1) - x(i+1)*yi) ]
(defun r90r:center (pts / n i p1 p2 cr a sx sy)
  (setq n (length pts) i 0 a 0.0 sx 0.0 sy 0.0)
  (while (< i n)
    (setq p1 (nth i pts)
          p2 (nth (rem (1+ i) n) pts)            ; last vertex -> first
          cr (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))
          a  (+ a cr)
          sx (+ sx (* (+ (car p1) (car p2)) cr))
          sy (+ sy (* (+ (cadr p1) (cadr p2)) cr))
          i  (1+ i)
    )
  )
  (setq a (* 0.5 a))
  (if (> (abs a) 1e-10)
    (list (/ sx (* 6.0 a)) (/ sy (* 6.0 a)))
    (r90r:avg pts)                                ; zero area -> average
  )
)

;; rotate one point by +/-90 deg around c
;; cos(90)=0 and sin(90)=1 are used literally -> the center is kept exact
(defun r90r:rot90 (p c s / dx dy)
  (setq dx (- (car p) (car c))
        dy (- (cadr p) (cadr c))
  )
  (list (- (car c) (* s dy))
        (+ (cadr c) (* s dx))
  )
)

;; closed by flag?  /  closed only geometrically (first point = last)?
(defun r90r:closed-flag (ed)
  (= 1 (logand 1 (cdr (assoc 70 ed))))
)
(defun r90r:closed-geom (pts / a b)
  (and (> (length pts) 2)
       (setq a (car pts) b (last pts))
       (< (distance (list (car a) (cadr a)) (list (car b) (cadr b))) 1e-8)
  )
)

;; supported polyline type?  ("" = ok, else reason)
(defun r90r:kind (ed / typ f)
  (setq typ (cdr (assoc 0 ed))
        f   (cdr (assoc 70 ed))
  )
  (cond
    ((= typ "LWPOLYLINE") "LW")
    ((/= typ "POLYLINE") nil)
    ((/= 0 (logand 88 f)) nil)     ; 8=3D poly, 16=mesh, 64=polyface
    (T "PL")
  )
)

;;; ------------------------------------------------------------------
;;;  low level modification (AutoLISP has no LET - plain defuns used)
;;; ------------------------------------------------------------------

;; LWPOLYLINE: rewrite every DXF 10 group of the entity list
(defun r90r:mod-lw (en ed cen s / new)
  (setq new (mapcar '(lambda (x)
                       (if (= 10 (car x))
                         (cons 10 (r90r:rot90 (cdr x) cen s))
                         x
                       )
                     )
                    ed
            )
  )
  (if (entmod new) (progn (entupd en) T))
)

;; heavy 2D POLYLINE: move every VERTEX sub-entity, keep its Z
(defun r90r:mod-pl (en raw cen s / ok v vd np z)
  (setq ok T)
  (foreach v raw
    (setq vd (entget (car v))
          z  (if (cadddr v) (cadddr v) 0.0)
          np (r90r:rot90 (cdr v) cen s)
          np (list (car np) (cadr np) z)
    )
    (if (null (entmod (subst (cons 10 np) (assoc 10 vd) vd)))
      (setq ok nil)
    )
  )
  (entupd en)
  ok
)

;;; ------------------------------------------------------------------
;;;  process one entity
;;;  returns: 'OK 'GEOM 'OPEN 'LOCK 'TYPE 'ERR
;;; ------------------------------------------------------------------
(defun r90r:do (en / ed knd pts raw cen nrm s res geom)
  (setq ed  (entget en)
        knd (r90r:kind ed)
  )
  (cond
    ((null knd) 'TYPE)
    ((r90r:locked ed) 'LOCK)
    (T
     (setq raw (if (= knd "LW") (r90r:verts-lw ed) (r90r:verts-pl en))
           pts (if (= knd "LW") raw (mapcar 'cdr raw))
     )
     (cond
       ((< (length pts) 3) 'ERR)
       ((not (or (r90r:closed-flag ed)
                 (setq geom (r90r:closed-geom pts))))
        'OPEN)
       (T
        (setq cen (r90r:center pts)
              nrm (cdr (assoc 210 ed))
        )
        ;; Vertices are stored in the OCS of the object, so rotating
        ;; them inside the OCS is exactly an in-plane rotation. With an
        ;; inverted extrusion (Z = -1) the OCS is mirrored as seen from
        ;; WCS +Z, so the sign is flipped to still give +90 on screen.
        (setq s (if (and nrm (< (caddr nrm) 0.0)) -1.0 1.0))
        (setq res (vl-catch-all-apply
                    (if (= knd "LW") 'r90r:mod-lw 'r90r:mod-pl)
                    (if (= knd "LW")
                      (list en ed cen s)
                      (list en raw cen s)
                    )
                  )
        )
        (cond
          ((vl-catch-all-error-p res) 'ERR)
          ((null res) 'ERR)                      ; entmod refused it
          (geom 'GEOM)
          (T 'OK)
        )
       )
     )
    )
  )
)

;;; ==================================================================
;;;  MAIN COMMAND
;;; ==================================================================
(defun c:R90R (/ *error* cme ss i en res ok geom opn lck typ bad undo)

  (setq cme (getvar "CMDECHO"))

  (defun *error* (msg)
    (if (> (getvar "CMDACTIVE") 0) (command))
    (if undo (command "_.UNDO" "_End"))
    (setvar "CMDECHO" cme)
    (cond
      ((null msg) nil)
      ((wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*")
       (princ "\nR90R: cancelled by user."))
      (T (princ (strcat "\nR90R: error - " msg)))
    )
    (princ)
  )

  (setvar "CMDECHO" 0)

  (princ "\nSelect polylines to rotate (window / crossing / ALL)...")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))

  (if (null ss)
    (princ "\nR90R: no LWPOLYLINE / POLYLINE in the selection - nothing done.\n      (run R90DIAG on the same objects to see what they really are)")
    (progn
      (command "_.UNDO" "_Begin")
      (setq undo T)

      (setq i 0 ok 0 geom 0 opn 0 lck 0 typ 0 bad 0)
      (while (setq en (ssname ss i))
        (setq res (r90r:do en))
        (cond
          ((eq res 'OK)   (setq ok   (1+ ok)))
          ((eq res 'GEOM) (setq geom (1+ geom)))
          ((eq res 'OPEN) (setq opn  (1+ opn)))
          ((eq res 'LOCK) (setq lck  (1+ lck)))
          ((eq res 'TYPE) (setq typ  (1+ typ)))
          (T              (setq bad  (1+ bad)))
        )
        (setq i (1+ i))
      )

      (command "_.UNDO" "_End")
      (setq undo nil)

      (princ (strcat "\nR90R: rotated by +90 deg : " (itoa (+ ok geom))))
      (if (> geom 0)
        (princ (strcat "\n      (of them closed by geometry, flag not set : "
                       (itoa geom) ")")))
      (if (> opn 0)
        (princ (strcat "\n      skipped, really OPEN      : " (itoa opn))))
      (if (> lck 0)
        (princ (strcat "\n      skipped, LAYER IS LOCKED  : " (itoa lck))))
      (if (> typ 0)
        (princ (strcat "\n      skipped, 3D/mesh polyline : " (itoa typ))))
      (if (> bad 0)
        (princ (strcat "\n      skipped, cannot modify    : " (itoa bad))))
      (if (and (= ok 0) (= geom 0))
        (princ "\n      nothing was rotated - run R90DIAG on the same objects."))
    )
  )

  (setvar "CMDECHO" cme)
  (princ)
)

;;; ==================================================================
;;;  DIAGNOSTICS
;;; ==================================================================
(defun c:R90DIAG (/ ss i en ed typ knd pts key lst cell)

  (princ "\nR90DIAG: select the objects you tried to rotate...")
  (setq ss (ssget))

  (if (null ss)
    (princ "\nR90DIAG: nothing selected.")
    (progn
      (setq i 0)
      (while (setq en (ssname ss i))
        (setq ed  (entget en)
              typ (cdr (assoc 0 ed))
              knd (r90r:kind ed)
              key typ
        )
        (if knd
          (progn
            (setq pts (if (= knd "LW")
                        (r90r:verts-lw ed)
                        (mapcar 'cdr (r90r:verts-pl en))
                      )
            )
            (setq key
              (strcat typ
                      " / " (itoa (length pts)) " vert / "
                      (cond ((r90r:closed-flag ed) "CLOSED (flag)")
                            ((r90r:closed-geom pts) "closed by geometry only")
                            (T "OPEN -> skipped")
                      )
              )
            )
          )
          (if (member typ '("POLYLINE"))
            (setq key (strcat typ " / 3D or mesh -> skipped"))
          )
        )
        (if (r90r:locked ed)
          (setq key (strcat key " + LAYER LOCKED -> skipped"))
        )
        (if (setq cell (assoc key lst))
          (setq lst (subst (cons key (1+ (cdr cell))) cell lst))
          (setq lst (cons (cons key 1) lst))
        )
        (setq i (1+ i))
      )

      (princ (strcat "\nR90DIAG: " (itoa (sslength ss)) " object(s) selected:"))
      (foreach cell (reverse lst)
        (princ (strcat "\n   " (itoa (cdr cell)) " x  " (car cell)))
      )
      (princ "\n   (INSERT = block reference: the polylines are inside a block,")
      (princ "\n    explode it or edit the block, R90R cannot reach them.)")
    )
  )
  (princ)
)

(princ "\nR90R.lsp loaded.  R90R = rotate each closed polyline +90 deg about its own center.")
(princ "\n                  R90DIAG = check why an object is not rotated.")
(princ)
