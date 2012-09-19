(in-package #:cl-poker-eval)

(declaim 
 (ftype (function (fixnum) fixnum) handval-hand-type) (inline handval-hand-type)
 (ftype (function (fixnum) fixnum) handval-top-card) (inline handval-top-card)
 (ftype (function (fixnum) fixnum) handval-second-card) (inline handval-second-card)
 (ftype (function (fixnum) fixnum) handval-third-card) (inline handval-third-card)
 (ftype (function (fixnum) fixnum) handval-fourth-card) (inline handval-fourth-card)
 (ftype (function (fixnum) fixnum) handval-fifth-card) (inline handval-fifth-card) 
 (ftype (function (fixnum) fixnum) handval-handtype-value) (inline handval-handtype-value)
 (ftype (function (fixnum) fixnum) handval-top-card-value) (inline handval-top-card-value)
 (ftype (function (fixnum) fixnum) handval-second-card-value) (inline handval-second-card-value)
 (ftype (function (fixnum) fixnum) handval-third-card-value) (inline handval-third-card-value)
 (ftype (function (fixnum) fixnum) handval-fourth-card-value) (inline handval-fourth-card-value)
 (ftype (function (fixnum) fixnum) handval-fifth-card-value) (inline handval-fifth-card-value)
 (ftype (function (fixnum) fixnum) get-top-card) (inline get-top-card))

(defconstant spades-mask (ash #b1111111111111 39) "For masking out all spades in a hand")
(defconstant hearts-mask  (ash #b1111111111111 26) "For masking out all hearts in a hand")
(defconstant diamonds-mask (ash #b1111111111111 13) "For masking out all diamonds in a hand")
(defconstant clubs-mask #b1111111111111 "For masking out all clubs in a hand")

(defconstant handval-nopair 0)
(defconstant handval-onepair 1)
(defconstant handval-twopair 2)
(defconstant handval-trips 3)
(defconstant handval-straight 4)
(defconstant handval-flush 5)
(defconstant handval-fullhouse 6)
(defconstant handval-quads 7)
(defconstant handval-stflush 8)

(defconstant handval-handtype-shift 24)
(defconstant handval-handtype-mask #x0F000000)
(defconstant handval-cards-shift 0)
(defconstant handval-cards-mask #x000FFFFF)
(defconstant handval-top-card-shift 16)
(defconstant handval-top-card-mask #x000F0000)
(defconstant handval-second-card-shift 12)
(defconstant handval-second-card-mask #x0000F000)
(defconstant handval-third-card-shift 8)
(defconstant handval-third-card-mask #x00000F00)
(defconstant handval-fourth-card-shift 4)
(defconstant handval-fourth-card-mask #x0000000F)
(defconstant handval-fifth-card-shift 0)
(defconstant handval-fifth-card-mask #x0000000F)
(defconstant handval-card-width 4)
(defconstant handval-card-mask #x0F)

(defun handval-hand-type (hv)
  (declare  (optimize (speed 3) (safety 0) (debug 0)) (type fixnum hv))
  (the fixnum (ash hv handval-handtype-shift)))

(defun handval-cards (hv)
  (declare
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum hv))
  (logand hv (- handval-cards-mask)))

(defun handval-top-card (hv)
  (declare 
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum hv))
  (logand (the fixnum (ash hv (- handval-top-card-shift)))  handval-card-mask))

(defun handval-second-card (hv)
  (declare
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum hv))
  (logand (ash hv (- handval-second-card-shift)) handval-card-mask))

(defun handval-third-card (hv)
  (declare
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum hv))
  (logand (ash hv (- handval-third-card-shift)) handval-card-mask))

(defun handval-fourth-card (hv)
  (declare
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum hv))
  (logand (ash hv (- handval-fourth-card-shift)) handval-card-mask))

(defun handval-fifthh-card (hv)
  (declare
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum hv))
  (logand (ash hv (- handval-fifth-card-shift)) handval-card-mask))

(defun handval-handtype-value (ht)
  (declare 
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum ht))
  (the fixnum (ash ht (the fixnum handval-handtype-shift))))

(defun handval-top-card-value (c)
  (declare 
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum c))
  (the fixnum (ash c (the fixnum handval-top-card-shift))))

(defun handval-second-card-value (c)
  (declare 
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum c))
  (the fixnum (ash c (the fixnum handval-second-card-shift))))

(defun handval-third-card-value (c)
  (declare 
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum c))
  (the fixnum (ash c (the fixnum handval-third-card-shift))))

(defun handval-fourth-card-value (c)
  (declare 
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum c))
  (the fixnum (ash c (the fixnum handval-fourth-card-shift))))

(defun handval-fifth-card-value (c)
  (declare 
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum c))
  (the fixnum (ash c (the fixnum handval-fifth-card-shift))))

(defun get-top-card (mask)
  (declare 
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum mask)
   (type (simple-array fixnum (8192)) top-card-tbl))
 (aref top-card-tbl mask))