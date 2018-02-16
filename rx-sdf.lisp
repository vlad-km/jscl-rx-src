;;; -*- mode:lisp; coding:utf-8  -*-

;;; JSCL-RX is a lisp (JSCL) wrapper for JSCL-RX
;;; This file is part of the JSCL-RX
;;; Copyright © 2018 Vladimir Mezentsev



(lores:defsys :rx
    :path  "git/jscl-rx"
    :depends (:promise)
    :components ((:file "rx-package")
                 (:module "static" :depends ("rx-package")
                          (:file "observable"))
                 (:module "instance" :depends ("rx-package")
                          (:file "observable"))
                 (:file "finality")))



;;; EOF
