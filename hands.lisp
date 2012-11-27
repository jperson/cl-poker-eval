(in-package #:cl-poker-eval)

(defun make-cards-masks (&rest cards)
  (let ((sh 0) (sd 0) (ss 0) (sc 0))
    (declare (type fixnum ss sh sd sc))
    (loop for c in cards do
          (cond
            ((< (the fixnum c) 13) (setf sc (the fixnum (logior (the fixnum sc) (the fixnum (ash 1 (the fixnum c)))))))
            ((< (the fixnum c) 26) (setf sd (the fixnum (logior (the fixnum sd) (the fixnum (ash 1 (the fixnum (- (the fixnum c) 13))))))))
            ((< (the fixnum c) 39) (setf sh (the fixnum (logior (the fixnum sh) (the fixnum (ash 1 (the fixnum (- (the fixnum c) 26))))))))
            (t (setf ss (the fixnum (logior (the fixnum ss) (the fixnum (ash 1 (- (the fixnum c) 39)))))))))
    (values ss sh sd sc)))

(defun make-hand-var (&rest cards)
  (multiple-value-bind (ss sh sd sc) (apply #'make-cards-masks cards)
   (logior (ash ss 39) (ash sh 26) (ash sd 13) sc)))

(defun get-cards (hand)
  (loop for i from 0 upto 51 
        if (< 0 (logand hand (ash 1 i))) collect i into c
        else collect i into deck
        finally (return (values c deck))))
