;; xsd-types-package.lisp

(in-package #:cl-user)

(defpackage  #:info.metacommunity.cltl.dnet.xsd.data-types
  (:nicknames #:dnet.xsd.data-types)
  (:use #:info.metacommunity.cltl.utils
        #:runes
        #:cl)
  (:export
   #:ncname-p
   #:ncname
   #:simple-ncname
   #:nmtoken-p
   #:nmtoken
   #:simple-nmtoken
   #:qname-p
   #:qname
   #:simple-qname
   ))


