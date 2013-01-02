;; Copyright (c) 2012, Jason R. Person
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution. 
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; The views and conclusions contained in the software and documentation are those
;; of the authors and should not be interpreted as representing official policies, 
;; either expressed or implied, of the FreeBSD Project.

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
