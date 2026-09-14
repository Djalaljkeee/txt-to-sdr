;;; ==================================================================
;;;  R90C.lsp
;;;  ------------------------------------------------------------------
;;;  Command : R90C
;;;  Purpose : Rotate EVERY selected CLOSED LWPOLYLINE by exactly
;;;            +90 degrees around ITS OWN geometric center.
;;;
;;;  Rules implemented:
;;;    * one common base point is NEVER used;
;;;    * the center is calculated separately for every polyline;
;;;    * the overall bounding box of the selection is NEVER used;
;;;    * after rotation the center of each polyline stays exactly
;;;      in the same point, therefore distances between objects and
;;;      their relative positions do not change;
;;;    * only CLOSED LWPOLYLINEs are processed, open ones are skipped;
;;;    * works with polylines rotated at any angle to the WCS.
;;;
;;;  Implementation note:
;;;    The vertices are rewritten directly with ENTMOD (DXF group 10),
;;;    so no ActiveX / no external libraries are required, the whole
;;;    operation is a single exact transformation and the vertex
;;;    order (internal orientation of the object) is updated as well -
;;;    which matters for squares, where the shape after a 90 deg turn
;;;    looks identical but the object itself must really be rotated.
;;;
;;;  Messages are kept in plain ASCII on purpose, so the file loads
;;;  correctly in any AutoCAD version regardless of file encoding.
;;; ==================================================================

;;; --- all vertices (DXF 10) of an LWPOLYLINE, in order -------------
(defun r90c:verts (ed / v lst)
  (foreach v ed
    (if (= 10 (car v))
      (setq lst (cons (cdr v) lst))
    )
  )
  (reverse lst)
)

;;; --- average of the vertices (fallback for degenerate shapes) -----
(defun r90c:avg (pts / sx sy n)
  (setq sx 0.0
        sy 0.0
        n  (length pts)
  )
  (foreach p pts
    (setq sx (+ sx (car p))
          sy (+ sy (cadr p))
    )
  )
  (list (/ sx n) (/ sy n))
)

;;; --- true geometric center (area centroid) of a closed polygon ----
;;;
;;;   A  = 1/2      * SUM[ xi*y(i+1) - x(i+1)*yi ]
;;;   Cx = 1/(6*A)  * SUM[ (xi + x(i+1)) * (xi*y(i+1) - x(i+1)*yi) ]
;;;   Cy = 1/(6*A)  * SUM[ (yi + y(i+1)) * (xi*y(i+1) - x(i+1)*yi) ]
;;;
;;; This is Green's theorem for the polygon, NOT a bounding box, so
;;; the result is correct for any rotation angle of the object.
;;; For a rectangle / square it is exactly the intersection point of
;;; the diagonals.
(defun r90c:center (pts / n i p1 p2 cr a sx sy)
  (setq n  (length pts)
        i  0
        a  0.0
        sx 0.0
        sy 0.0
  )
  (while (< i n)
    (setq p1 (nth i pts)
          p2 (nth (rem (1+ i) n) pts)          ; last vertex -> first
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
    (r90c:avg pts)                              ; zero area -> average
  )
)

;;; --- rotate one point by +/-90 deg around c -----------------------
;;; cos(90) = 0 and sin(90) = 1 are used literally, so there is no
;;; floating point dust: the center is preserved bit-exactly.
(defun r90c:rot90 (p c s / dx dy)
  (setq dx (- (car p) (car c))
        dy (- (cadr p) (cadr c))
  )
  (list (- (car c) (* s dy))
        (+ (cadr c) (* s dx))
  )
)

;;; --- process one entity -------------------------------------------
;;; returns: T = rotated, "OPEN" / "DEGEN" / "ERR" = skipped
(defun r90c:do (en / ed pts cen nrm s new)
  (setq ed (entget en))
  (cond
    ((/= "LWPOLYLINE" (cdr (assoc 0 ed))) "ERR")
    ((/= 1 (logand 1 (cdr (assoc 70 ed)))) "OPEN")   ; not closed
    ((< (length (setq pts (r90c:verts ed))) 3) "DEGEN")
    (T
      (setq cen (r90c:center pts)
            nrm (cdr (assoc 210 ed))
      )
      ;; Vertices of an LWPOLYLINE are stored in the OCS of the object.
      ;; Rotating them inside the OCS is exactly an in-plane rotation
      ;; of the object. If the extrusion direction is inverted
      ;; (Z = -1), the OCS is mirrored when seen from the WCS +Z, so
      ;; the sign of the angle has to be flipped to still get +90 deg
      ;; on screen.
      (setq s (if (and nrm (< (caddr nrm) 0.0)) -1.0 1.0))
      (setq new (mapcar '(lambda (x)
                           (if (= 10 (car x))
                             (cons 10 (r90c:rot90 (cdr x) cen s))
                             x
                           )
                         )
                        ed
                )
      )
      (if (vl-catch-all-error-p
            (vl-catch-all-apply 'entmod (list new)))
        "ERR"                                   ; locked layer, etc.
        (progn (entupd en) T)
      )
    )
  )
)

;;; ==================================================================
;;;  MAIN COMMAND
;;; ==================================================================
(defun c:R90C (/ *error* cme ss i en res cnt opn bad undo)

  (setq cme (getvar "CMDECHO"))

  (defun *error* (msg)
    (if undo (command "_.UNDO" "_End"))
    (if cme (setvar "CMDECHO" cme))
    (cond
      ((null msg) nil)
      ((wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*")
       (princ "\nR90C: cancelled by user."))
      (T (princ (strcat "\nR90C: error - " msg)))
    )
    (princ)
  )

  (setvar "CMDECHO" 0)

  (princ "\nSelect closed polylines to rotate (window / crossing / ALL)...")
  (setq ss (ssget '((0 . "LWPOLYLINE"))))       ; filter: LWPOLYLINE only

  (if (null ss)
    (princ "\nR90C: nothing selected - command cancelled.")
    (progn
      (command "_.UNDO" "_Begin")
      (setq undo T)

      (setq i   0
            cnt 0                                ; rotated
            opn 0                                ; skipped: not closed
            bad 0                                ; skipped: error / bad
      )
      (while (setq en (ssname ss i))
        (setq res (r90c:do en))
        (cond
          ((eq res T)      (setq cnt (1+ cnt)))
          ((= res "OPEN")  (setq opn (1+ opn)))
          (T               (setq bad (1+ bad)))
        )
        (setq i (1+ i))
      )

      (command "_.UNDO" "_End")
      (setq undo nil)

      (princ (strcat "\nR90C: rotated by +90 deg : " (itoa cnt)))
      (if (> opn 0)
        (princ (strcat "\n      skipped (not closed) : " (itoa opn)))
      )
      (if (> bad 0)
        (princ (strcat "\n      skipped (locked/invalid) : " (itoa bad)))
      )
    )
  )

  (setvar "CMDECHO" cme)
  (princ)
)

(princ "\nR90C.lsp loaded.  Type R90C to rotate each closed LWPOLYLINE +90 deg about its own center.")
(princ)
