;; info.metacommunity.cltl.dnet.xsd.data-types.asd		-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:dnet-system
    (:use #:asdf #:cl)))

(in-package #:dnet-system)


(defsystem #:info.metacommunity.cltl.dnet.xsd.data-types
  :description 
  "Implementation of XML Schema Data types"
  :version "1.0"
  :homepage "https://github.com/MetaCommunity/mci-dnet-xsd"
  :license "https://github.com/MetaCommunity/mci-dnet-xsd/blob/master/LICENSE"
  :depends-on (#:info.metacommunity.cltl.dnet.uri
               #:info.metacommunity.cltl.utils
               #:closure-common)
  ;; NB: Due to the direct dependency onto #:closure-common, this
  ;; system also depends indirectly onto #:trivial-gray-strams (#-scl) 
  ;; and #:babel (#+rune-is-character)
  :components 
  ((:file "xsd-types-package")
   (:file "xml-names"
          :depends-on ("xsd-types-package")
          )
   #+NIL 
   (:file ""
          :depends-on ("xsd-types-package"))
   ))
