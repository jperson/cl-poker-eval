(in-package #:cl-poker-eval)

(declaim 
 (ftype (function (fixnum fixnum) fixnum) eval-twopair) (inline eval-twopair)
 (ftype (function (fixnum fixnum) fixnum) eval-twopair-2) (inline eval-twopair-2)
 (ftype (function (fixnum fixnum) fixnum) eval-trips) (inline eval-trips)
 (ftype (function (fixnum fixnum) fixnum) eval-fullhouse) (inline eval-fullhouse)
 (ftype (function (fixnum fixnum) fixnum) eval-quads) (inline eval-quads)
 (inline eval-hand))

(defun eval-twopair (mask hranks)
  (declare 
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum mask hranks handval-top-card-mask handval-second-card-mask)
   (type (simple-array fixnum (8192)) top-five-cards-tbl))
  (the fixnum 
    (+ (handval-handtype-value handval-twopair)
       (logand (aref top-five-cards-tbl mask) (logior handval-top-card-mask handval-second-card-mask))
       (handval-third-card-value (get-top-card (logxor hranks mask))))))

(defmacro handtype-topcard-value (hv tpc)
  (values 
   `(the fixnum (handval-handtype-value (the fixnum ,hv)))
   `(the fixnum (handval-top-card-value (the fixnum ,tpc)))))

(defun eval-twopair-2 (mask hranks)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((tc-mask (the fixnum (ash 1 (get-top-card mask))))
	 (xormask (get-top-card (logxor mask tc-mask))))
    (declare (type fixnum tc-mask xormask))
    (the fixnum 
      (+ (handtype-topcard-value handval-twopair (get-top-card mask))
	     (handval-second-card-value xormask)
	     (handval-third-card-value (get-top-card (logxor hranks tc-mask (the fixnum (ash 1 xormask)))))))))

(defun eval-trips (three-mask ranks)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((2cv (get-top-card (logxor ranks three-mask)))
	 (3cv (get-top-card (logxor ranks three-mask (the fixnum (ash 1 2cv))))))
    (declare (type fixnum 2cv 3cv))
    (the fixnum 
      (+ (handtype-topcard-value handval-trips (get-top-card three-mask))
	 (handval-second-card-value 2cv)
	 (handval-third-card-value 3cv)))))

(defun eval-fullhouse (mask1 mask2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((top-card (get-top-card mask1)))
    (the fixnum 
      (+ (handtype-topcard-value handval-fullhouse top-card)
	 (handval-second-card-value (get-top-card (logxor (logior mask1 mask2) (the fixnum (ash 1 top-card)))))))))

(defmacro make-card-masks ((ss sh sd sc &rest cards) &body body)
  `(let ((sh 0) (sd 0) (ss 0) (sc 0))
     (declare (type fixnum ss sh sd sc))
     ,@(loop for c in cards append
	    (list `(cond
		     ((< (the fixnum ,c) 13) (setf ,sc (the fixnum (logior (the fixnum ,sc) (the fixnum (ash 1 (the fixnum ,c)))))))
		     ((< (the fixnum ,c) 26) (setf ,sd (the fixnum (logior (the fixnum ,sd) (the fixnum (ash 1 (the fixnum (- (the fixnum ,c) 13))))))))
		     ((< (the fixnum ,c) 39) (setf ,sh (the fixnum (logior (the fixnum ,sh) (the fixnum (ash 1 (the fixnum (- (the fixnum ,c) 26))))))))
		     (t (setf ,ss (the fixnum (logior (the fixnum ,ss) (the fixnum (ash 1 (- (the fixnum ,c) 39)))))))))
	  into v finally (return v))
  ,@body))

(defun eval-quads (mask hranks)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum 
    (+ (handtype-topcard-value handval-quads (get-top-card mask))
       (handval-second-card-value (get-top-card (logxor hranks (the fixnum (ash 1 (get-top-card mask)))))))))

(defmacro str-strflush-value (dups rank &rest suit-masks)
  `(cond
     ,@(loop for sm in suit-masks append
	    (list `((> (logcount (the (unsigned-byte 32) ,sm)) 4)
		    (let ((st (the fixnum (aref straight-table ,sm))))
		      (if (> st 0)
			  (return-from eval-hand
			    (the fixnum (+ (handval-handtype-value handval-stflush) (handval-top-card-value st))))
			  (if (< ,dups 3)
			      (the fixnum (+ (handval-handtype-value handval-flush) (aref top-five-cards-tbl st)))))))) into v finally (return v))
     ((> (aref straight-table ,rank) 0) 
      (return-from eval-hand (the fixnum (+ (handval-handtype-value handval-straight) (handval-top-card-value (the fixnum (aref straight-table ,rank)))))))))

(defun eval-hand-7 (c1 c2 c3 c4 c5 c6 c7)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (make-card-masks (ss sh sc sd c1 c2 c3 c4 c5 c6 c7)
    (eval-hand 7 ss sh sc sd)))

(defun eval-hand (n-cards ss sh sc sd)
  (declare 
   (optimize (speed 3) (safety 0) (debug 0))
   (type fixnum n-cards ss sh sc sd)
   (type (simple-array fixnum (8192)) nbits-tbl))
  (let* ((ranks (the fixnum (logior ss sh sd sc)))
	 (n-ranks (the fixnum (aref nbits-tbl ranks)))
	 (n-dups (the fixnum (- n-cards n-ranks))))
    (declare 
     (type fixnum ranks n-ranks n-dups)
     (type (simple-array fixnum (8192)) straight-table)
     (type (simple-array fixnum (8192)) top-five-cards-tbl)
     (type (simple-array fixnum (8192)) top-card-tbl))
    (if (> n-ranks 5)
	    (str-strflush-value n-dups ranks ss sh sc sd))
    (let ((two-mask (the fixnum (logxor ranks (logxor sc sd sh ss)))))
      (declare (type fixnum two-mask))
      (return-from eval-hand
	(case n-dups
	  (0
	   (the fixnum (+ (handval-handtype-value handval-nopair) (aref top-five-cards-tbl ranks))))
	  (1 
	   (let ((retval (the fixnum (+ (handval-handtype-value handval-onepair) (handval-top-card-value (get-top-card two-mask)))))
		 (kickers (logand (the fixnum (ash (aref top-five-cards-tbl (logxor ranks two-mask))  (- handval-card-width))) (lognot handval-fifth-card-mask))))
	     (declare (type fixnum retval kickers))
	     (the fixnum (+  retval kickers))))
	  (2 
	   (if (> two-mask 0)
	       (eval-twopair two-mask ranks)
	       (let ((three-mask (logand (logior (logand sc sd) (logand sh ss)) (logior (logand sc sh) (logand sd ss)))))
		 (declare (type fixnum three-mask))
		 (eval-trips three-mask ranks))))
	  (otherwise 
	   (let ((four-mask (logand sh sd sc ss)))
	     (declare (type fixnum four-mask)) 
	     (if (not (= four-mask 0))
		 (eval-quads four-mask ranks)
		 (if (not (= (the fixnum (aref nbits-tbl two-mask)) n-dups))
		     (eval-fullhouse (the fixnum (logand (logior (logand sc sd) (logand sh ss)) (logior (logand sc sh) (logand sd ss)))) two-mask)
		     (eval-twopair-2 two-mask ranks))))))))))
