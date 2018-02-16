;;; -*- mode:lisp; coding:utf-8  -*-

;;; JSCL-RX is a Lisp (JSCL) wrapper for RxJS
;;; Copyright © 2018 Vladimir Mezentsev
;;;
;;; JSCL-RX is free software: you can redistribute it and/or modify it under
;;; the terms of the GNU General  Public License as published by the Free
;;; Software Foundation,  either version  3 of the  License, or  (at your
;;; option) any later version.
;;;
;;; JSCL-RX is distributed  in the hope that it will  be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should  have received a  copy of  the GNU General  Public License
;;; Version 3 from  <http://www.gnu.org/licenses/>.
;;;
;;;
;;; JSCL - Is a Common Lisp to Javascript compiler
;;;        https://github.com/jscl-project/jscl
;;;        Licensed under GNU General Public License v3.0
;;;
;;; RxJS - The Reactive Extensions for JavaScript (RxJS) 4.0
;;;        https://github.com/Reactive-Extensions/RxJS
;;;        Licensed under the Apache License, Version 2.0
;;;



(eval-when (:compile-toplevel :load-toplevel :execute)
    (unless (find-package :rx)
        (make-package :rx :use (list 'cl))))


(in-package :rx)
(export '(jscl::new jscl::oget jscl::make-new jscl::concat jscl::fset))
(export '(jscl::list-to-vector))
(in-package :cl-user)



;;; EOF
