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

;;;; cl-poker-eval.asd
(asdf:defsystem #:cl-poker-eval
  :author "Jason Person"
  :license "BSD"
  :description "7-card hand poker evaluator"
  :version "0.1"
  :components ((:file "package")
               (:file "cl-poker-eval"   :depends-on ("package" "poker-defs" "straight-tbl" "hands"))
               (:file "top-5-cards-tbl" :depends-on ("package" "poker-defs"))
               (:file "straight-tbl"    :depends-on ("package" "poker-defs"))
               (:file "top-card-tbl"    :depends-on ("package" "poker-defs"))
               (:file "poker-defs"      :depends-on ("package")) 
               (:file "nbits-tbl"       :depends-on ("package"))
               (:file "hands"           :depends-on ("package"))))

