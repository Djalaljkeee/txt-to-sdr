;;; ==================================================================
;;;  R90R.lsp
;;;  ------------------------------------------------------------------
;;;  Commands :
;;;     R90R    - rotate EVERY selected closed polyline by exactly
;;;               +45 degrees around ITS OWN geometric center.
;;;               The angle is set by *R90R-DEG* below - change that
;;;               single line to use 90, 30, -45 ... instead.
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

;;; ==================================================================
;;;  ROTATION ANGLE, DEGREES, COUNTERCLOCKWISE.
;;;  Change this one line to rotate by something else (90, 30, -45...)
;;; ==================================================================
(setq *R90R-DEG* 45.0)

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

;; cosine/sine of an angle given in degrees.
;; Multiples of 90 use exact 0 / +-1, so those turns stay free of
;; floating point dust and the center is preserved bit-exactly.
(defun r90r:cs (deg / m a)
  (setq m (rem (+ (rem deg 360.0) 360.0) 360.0))
  (cond
    ((equal m 0.0   1e-9) '(1.0 . 0.0))
    ((equal m 90.0  1e-9) '(0.0 . 1.0))
    ((equal m 180.0 1e-9) '(-1.0 . 0.0))
    ((equal m 270.0 1e-9) '(0.0 . -1.0))
    (T (setq a (/ (* pi m) 180.0)) (cons (cos a) (sin a)))
  )
)

;; angle actually used by the command (falls back to 45 deg)
(defun r90r:deg ()
  (if (and *R90R-DEG* (numberp *R90R-DEG*)) (float *R90R-DEG*) 45.0)
)

;; rotate one point around c by the angle given as cos/sin
(defun r90r:rotpt (p c ca sa / dx dy)
  (setq dx (- (car p) (car c))
        dy (- (cadr p) (cadr c))
  )
  (list (+ (car c) (- (* dx ca) (* dy sa)))
        (+ (cadr c) (+ (* dx sa) (* dy ca)))
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

;; side lengths of the contour (closed)
(defun r90r:sides (pts / n i lst p1 p2)
  (setq n (length pts) i 0)
  (while (< i n)
    (setq p1 (nth i pts)
          p2 (nth (rem (1+ i) n) pts)
          lst (cons (distance (list (car p1) (cadr p1))
                              (list (car p2) (cadr p2)))
                    lst)
          i (1+ i)
    )
  )
  ;; drop zero-length closing segment (duplicated last vertex)
  (vl-remove-if '(lambda (d) (< d 1e-8)) (reverse lst))
)

;; "SQUARE" / "RECTANGLE" / "OTHER"
(defun r90r:shape (pts / sd mn mx)
  (setq sd (r90r:sides pts))
  (cond
    ((/= 4 (length sd)) "OTHER")
    (T
     (setq mn (apply 'min sd) mx (apply 'max sd))
     (if (< (- mx mn) (* 1e-6 mx)) "SQUARE" "RECTANGLE")
    )
  )
)

;; angle of the first side, degrees 0..360
(defun r90r:edgeang (pts / a)
  (setq a (angle (list (caar pts) (cadar pts))
                 (list (car (cadr pts)) (cadr (cadr pts)))))
  (/ (* 180.0 a) pi)
)

;; readable point
(defun r90r:ptstr (p)
  (strcat "(" (rtos (car p) 2 4) ", " (rtos (cadr p) 2 4) ")")
)

;;; ------------------------------------------------------------------
;;;  low level modification (AutoLISP has no LET - plain defuns used)
;;; ------------------------------------------------------------------

;; LWPOLYLINE: rewrite every DXF 10 group of the entity list
(defun r90r:mod-lw (en ed cen ca sa / new)
  (setq new (mapcar '(lambda (x)
                       (if (= 10 (car x))
                         (cons 10 (r90r:rotpt (cdr x) cen ca sa))
                         x
                       )
                     )
                    ed
            )
  )
  (if (entmod new) (progn (entupd en) T))
)

;; heavy 2D POLYLINE: move every VERTEX sub-entity, keep its Z
(defun r90r:mod-pl (en raw cen ca sa / ok v vd np z)
  (setq ok T)
  (foreach v raw
    (setq vd (entget (car v))
          z  (if (cadddr v) (cadddr v) 0.0)
          np (r90r:rotpt (cdr v) cen ca sa)
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
(defun r90r:do (en / ed knd pts raw cen nrm s cs res geom)
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
        ;; WCS +Z, so the sign is flipped to still turn counterclockwise
        ;; on screen.
        (setq s  (if (and nrm (< (caddr nrm) 0.0)) -1.0 1.0)
              cs (r90r:cs (* s (r90r:deg)))
        )
        (setq res (vl-catch-all-apply
                    (if (= knd "LW") 'r90r:mod-lw 'r90r:mod-pl)
                    (if (= knd "LW")
                      (list en ed cen (car cs) (cdr cs))
                      (list en raw cen (car cs) (cdr cs))
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
(defun c:R90R (/ *error* cme deg ss i en res ok geom opn lck typ bad undo)

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
  (setq deg (r90r:deg))

  (princ (strcat "\nSelect polylines to rotate by "
                 (rtos deg 2 2)
                 " deg about their own centers (window / crossing / ALL)..."))
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

      (princ (strcat "\nR90R: rotated by " (rtos deg 2 2) " deg : "
                     (itoa (+ ok geom))))
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
(defun c:R90DIAG (/ ss i en ed typ knd pts key lst cell
                    sq rc ot amin amax a first-pt)

  (princ "\nR90DIAG: select the objects you tried to rotate...")
  (setq ss (ssget))

  (if (null ss)
    (princ "\nR90DIAG: nothing selected.")
    (progn
      (setq i 0 sq 0 rc 0 ot 0)
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
            (if (> (length pts) 2)
              (progn
                (if (null first-pt) (setq first-pt (car pts)))
                (setq a (r90r:edgeang pts))
                (if (or (null amin) (< a amin)) (setq amin a))
                (if (or (null amax) (> a amax)) (setq amax a))
                (cond
                  ((= "SQUARE"    (r90r:shape pts)) (setq sq (1+ sq)))
                  ((= "RECTANGLE" (r90r:shape pts)) (setq rc (1+ rc)))
                  (T (setq ot (1+ ot)))
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

      (if (assoc "INSERT" lst)
        (princ "\n   INSERT = block reference: the polylines are inside a block,\n          explode it or edit the block - R90R cannot reach them.")
      )

      (if (> (+ sq rc ot) 0)
        (progn
          (princ "\n   shape check:")
          (if (> sq 0)
            (princ (strcat "\n      SQUARES (all sides equal) : " (itoa sq)
                           (if (equal 0.0 (rem (r90r:deg) 90.0) 1e-9)
                             "   <- a 90 deg turn is INVISIBLE, the shape maps onto itself"
                             "   <- visible with the current angle"))))
          (if (> rc 0)
            (princ (strcat "\n      rectangles                : " (itoa rc)
                           "   <- the turn must be clearly visible")))
          (if (> ot 0)
            (princ (strcat "\n      other contours            : " (itoa ot))))
          (princ (strcat "\n      first side angle : from "
                         (rtos amin 2 2) " to " (rtos amax 2 2) " deg"))
        )
      )
      (if first-pt
        (progn
          (princ (strcat "\n   PROOF: 1st vertex of the 1st object = "
                         (r90r:ptstr first-pt)))
          (princ "\n          run R90R, then R90DIAG again on the same object:")
          (princ "\n          this point MUST change -> the rotation really happened.")
        )
      )
    )
  )
  (princ)
)

(princ (strcat "\nR90R.lsp loaded.  R90R = rotate each closed polyline "
               (rtos (r90r:deg) 2 2)
               " deg about its own center."))
(princ "\n                  R90DIAG = check why an object is not rotated.")
(princ)
