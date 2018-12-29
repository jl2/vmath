;;;; vmath.lisp 
;;
;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :vmath)

(declaim (optimize (speed 3)))
(deftype point ()
  '(simple-array double-float (3)))

(deftype rvector ()
  '(simple-array double-float (4)))

(deftype color ()
  '(simple-array double-float (4)))

(declaim (inline
          make-point
          make-rvector
          make-vect
          make-vect-4 make-rvector-4
          px py pz
          vx vy vz
          red green blue alpha
          (setf red) (setf green) (setf blue) (setf alpha)
          (setf px) (setf py) (setf pz)
          (setf vx) (setf vy) (setf vz)
          psub pvadd
          vsub vadd dot cross vscale
          vlen vlen-squared
          vnormalize
          make-rgb
          make-rgba
          cadd csub cmul cscale
          cclamp)
         
         (ftype (function (point) double-float) px py pz)
         (ftype (function (rvector) double-float) vx vy vz)
         (ftype (function (point double-float) double-float) (setf px) (setf py) (setf pz))
         (ftype (function (rvector double-float) double-float) (setf vx) (setf vy) (setf vz))

         (ftype (function (double-float double-float double-float) point) make-point)
         (ftype (function (double-float double-float double-float) rvector) make-rvector)
         (ftype (function (double-float double-float double-float) rvector) make-vect)
         (ftype (function (double-float double-float double-float double-float) rvector) make-rvector-4)
         (ftype (function (double-float double-float double-float double-float) rvector) make-vect-4)
         (ftype (function (point point) rvector) psub)
         (ftype (function (point rvector) point) pvadd)
         (ftype (function (rvector rvector) rvector) vadd vsub cross)
         (ftype (function (rvector rvector) double-float) dot)
         (ftype (function (double-float rvector) rvector) vscale)
         (ftype (function (rvector) (double-float 0.0 *)) vlen vlen-squared)
         (ftype (function (rvector) rvector) vnormalize)

         (ftype (function (color color) color) cadd csub cmul)
         (ftype (function (color) color) cclamp)
         (ftype (function (double-float color) color) cscale)
         (ftype (function (color) double-float) red green blue alpha))

(defun px (v)
  (declare (type point v))
  (aref v 0))
(defun (setf px) (v value)
  (declare (type point v)
           (type double-float value))
  (setf (aref v 0) value))

(defun py (v)
  (declare (type point v))
  (aref v 1))
(defun (setf py) (v value)
  (declare (type point v)
           (type double-float value))
  (setf (aref v 1) value))

(defun pz (v)
  (declare (type point v))
  (aref v 2))
(defun (setf pz) (v value)
  (declare (type point v)
           (type double-float value))
  (setf (aref v 2) value))

(defun make-point (x y z)
  (declare (type double-float x y z))
  (let ((rval (make-array '(3) :element-type 'double-float :initial-element 1.0d0)))
    (setf (aref rval 0) x)
    (setf (aref rval 1) y)
    (setf (aref rval 2) z)
    rval))

(defun make-rvector (x y z)
  (declare (type double-float x y z))
  (let ((rval (make-array '(4) :element-type 'double-float :initial-element 1.0d0)))
    (setf (aref rval 0) x)
    (setf (aref rval 1) y)
    (setf (aref rval 2) z)
    rval))
(defun make-rvector-4 (x y z w)
  (declare (type double-float x y z w))
  (let ((rval (make-array '(4) :element-type 'double-float :initial-element 0.0d0)))
    (setf (aref rval 0) x)
    (setf (aref rval 1) y)
    (setf (aref rval 2) z)
    (setf (aref rval 3) w)
    rval))

(defun make-vect-4 (x y z w)
  (declare (type double-float x y z w))
  (make-rvector-4 x y z w))

(defun make-vect (x y z)
  (declare (type double-float x y z))
  (make-rvector x y z))


  

(declaim (inline peq veq ceq)
         (ftype (function (point point) boolean) peq)
         (ftype (function (rvector rvector) boolean) veq)
         (ftype (function (color color) boolean) ceq))

(defun peq (p1 p2)
  (declare (type point p1 p2))
  (and (= (px p1) (px p2))
       (= (py p1) (py p2))
       (= (pz p1) (pz p2))))

(defun veq (v1 v2)
  (declare (type rvector v1 v2))
  (and (= (vx v1) (vx v2))
       (= (vy v1) (vy v2))
       (= (vz v1) (vz v2))))

(defun ceq (c1 c2)
  (declare (type color c1 c2))
  (and (= (red c1) (red c2))
       (= (green c1) (green c2))
       (= (blue c1) (blue c2))
       (= (alpha c1) (alpha c2))))

(defun vx (v)
  (declare (type rvector v))
  (aref v 0))
(defun (setf vx) (v value)
  (declare (type rvector v)
           (type double-float value))
  (setf (aref v 0) value))

(defun vy (v)
  (declare (type rvector v))
  (aref v 1))
(defun (setf vy) (v value)
  (declare (type rvector v)
           (type double-float value))
  (setf (aref v 1) value))

(defun vz (v)
  (declare (type rvector v))
  (aref v 2))
(defun (setf vz) (v value)
  (declare (type rvector v)
           (type double-float value))
  (setf (aref v 2) value))

(defun vw (v)
  (declare (type rvector v))
  (aref v 3))
(defun (setf vw) (v value)
  (declare (type rvector v)
           (type double-float value))
  (setf (aref v 3) value))

(defun vscale (k v)
  (declare (type double-float k)
           (type rvector v))
  (make-rvector (* k (vx v))
                (* k (vy v))
                (* k (vz v))))

(defun psub (p1 p2)
  (declare (type point p1 p2))
  (make-rvector (- (px p1) (px p2))
                (- (py p1) (py p2))
                (- (pz p1) (pz p2))))

(defun vsub (v1 v2)
  (declare (type rvector v1 v2))
  (make-rvector (- (vx v1) (vx v2))
                (- (vy v1) (vy v2))
                (- (vz v1) (vz v2))))

(defun vadd (v1 v2)
  (declare (type rvector v1 v2))
  (make-rvector (+ (vx v1) (vx v2))
                (+ (vy v1) (vy v2))
                (+ (vz v1) (vz v2))))

(defun pvadd (p v)
  (declare (type point p)
           (type rvector v))
  (make-point (+ (px p) (vx v))
                (+ (py p) (vy v))
                (+ (pz p) (vz v))))

(defun dot (v1 v2)
  (declare (type rvector v1 v2))
  (+ (* (vx v1) (vx v2))
     (* (vy v1) (vy v2))
     (* (vz v1) (vz v2))))

(defun cross (v1 v2)
  (declare (type rvector v1 v2))
  (let ((v1x (vx v1))
        (v1y (vy v1))
        (v1z (vz v1))
        (v2x (vx v2))
        (v2y (vy v2))
        (v2z (vz v2)))
    (make-rvector (- (* v1y v2z) (* v1z v2y))
                  (- (* v1z v2x) (* v1x v2z))
                  (- (* v1x v2y) (* v1y v2x)))))

(defun vlen (vect)
  (declare (type rvector vect))
  (let ((xl (vx vect))
        (yl (vy vect))
        (zl (vz vect)))
    (the (double-float 0.0) (sqrt (the (double-float 0.0 *) (+ (* xl xl) (* yl yl) (* zl zl)))))))

(defun vlen-squared (vect)
  (declare (type rvector vect))
  (let ((xl (vx vect))
        (yl (vy vect))
        (zl (vz vect)))
    (+ (* xl xl) (* yl yl) (* zl zl))))

(defun vnormalize (vect)
  (declare (type rvector vect))
  (let ((ilen (/ 1.0 (vlen vect))))
    (vscale ilen vect)))


(defun make-rgb (r g b)
  (declare (type double-float r g b))
  (let ((rval (make-array '(4) :element-type 'double-float :initial-element 0.0)))
    (setf (aref rval 0) r)
    (setf (aref rval 1) g)
    (setf (aref rval 2) b)
    (setf (aref rval 3) 1.0)
    rval))

(defun make-rgba (r g b a)
  (declare (type double-float r g b a))
  (let ((rval (make-array '(4) :element-type 'double-float :initial-element 0.0)))
    (setf (aref rval 0) r)
    (setf (aref rval 1) g)
    (setf (aref rval 2) b)
    (setf (aref rval 3) a)
    rval))

(defun red (color)
  (declare (type color color))
  (aref color 0))
(defun (setf red) (color value)
  (declare (type color color)
           (type double-float value))
  (setf (aref color 0) value))

(defun green (color)
  (declare (type color color))
  (aref color 1))
(defun (setf green) (color value)
  (declare (type color color)
           (type double-float value))
  (setf (aref color 1) value))

(defun blue (color)
  (declare (type color color))
  (aref color 2))
(defun (setf blue) (color value)
  (declare (type color color)
           (type double-float value))
  (setf (aref color 2) value))

(defun alpha (color)
  (declare (type color color))
  (aref color 3))
(defun (setf alpha) (color value)
  (declare (type color color)
           (type double-float value))
  (setf (aref color 3) value))

(defun cscale (k c)
  (declare (type double-float k)
           (type color c))
  (make-rgba (* k (red c))
             (* k (green c))
             (* k (blue c))
             (* k (alpha c))))

(defun cadd (c1 c2)
  (declare (type color c1 c2))
  (make-rgba (+ (red c1) (red c2))
             (+ (green c1) (green c2))
             (+ (blue c1) (blue c2))
             (+ (alpha c1) (alpha c2))))

(defun cmul (c1 c2)
  (declare (type color c1 c2))
  (make-rgba (* (red c1) (red c2))
             (* (green c1) (green c2))
             (* (blue c1) (blue c2))
             (* (alpha c1) (alpha c2))))

(defun csub (c1 c2)
  (declare (type color c1 c2))
  (make-rgba (- (red c1) (red c2))
             (- (green c1) (green c2))
             (- (blue c1) (blue c2))
             (- (alpha c1) (alpha c2))))
