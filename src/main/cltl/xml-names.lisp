;; xml-names.lisp - parsing for XML names

;;
;; Reference Resources:
;; [1] Namespaces in XML 1.0 (Third Edition)
;;     http://www.w3.org/TR/REC-xml-names/
;; [2] Extensible Markup Language (XML) 1.0 (Fifth Edition)
;;     http://www.w3.org/TR/REC-xml/
;;

(in-package #:dnet.xsd.data-types)

(deftype cdesignator ()
  #+RUNE-IS-CHARACTER
  '(or rune character-code)
  #-RUNE-IS-CHARACTER
  '(or character rune))

(deftype ccode ()
  #+RUNE-IS-CHARACTER
  'character-code
  #-RUNE-IS-CHARACTER
  'rune)


(defun ccode (c)
  (declare (type cdesignator c)
           (values ccode))
  #+RUNE-IS-CHARACTER
  (etypecase c
    (ccode c)
    (rune (rune-code c)))
  #-RUNE-IS-CHARACTER
  (etypecase c
    (rune c)
    (character (character-code c))))

(defmacro ccode-in-range (c min max)
  `(<= ,(ccode min) ,c ,(ccode max)))


(defun ncname-start-char-p (c)
  ;; http://www.w3.org/TR/REC-xml/#NT-NameStartChar  ;; excluding ":"
  ;; cf. http://www.w3.org/TR/REC-xml-names/#NT-NCName
  (declare (type cdesignator c)
           (inline ccode <= =)
           (values boolean))
  (let ((%c (ccode c)))
    (declare (type character-code %c))
    (or  (ccode-in-range %c #\a #\z)
         (ccode-in-range %c #\A #\Z)
         (= %c #.(char-code #\_))
         (ccode-in-range %c #xC0 #xD6)
         (ccode-in-range %c #xD8 #xF6) 
         (ccode-in-range %c #xF8 #x2FF)  
         (ccode-in-range %c #x370 #x37D)  
         (ccode-in-range %c #x37F #x1FFF)  
         (ccode-in-range %c #x200C #x200D)  
         (ccode-in-range %c #x2070 #x218F)  
         (ccode-in-range %c #x2C00 #x2FEF)  
         (ccode-in-range %c #x3001 #xD7FF)  
         (ccode-in-range %c #xF900 #xFDCF)  
         (ccode-in-range %c #xFDF0 #xFFFD)  
         (ccode-in-range %c #x10000 #xEFFFF))))

(defun ncname-char-p (c)
  ;; http://www.w3.org/TR/REC-xml/#NT-NameChar
  ;; excluding ":"
  ;; cf. http://www.w3.org/TR/REC-xml-names/#NT-NCName
  (declare (type cdesignator c)
           (inline ccode <= =)
           (values boolean))
  (let ((%c (ccode c)))
    (declare (type character-code %c))
    (or  (ccode-in-range %c #\a #\z)
         (ccode-in-range %c #\A #\Z)
         (= %c #.(char-code #\-))
         (= %c #.(char-code #\.))
         (= %c #.(char-code #\_))
         (ccode-in-range %c #\0 #\9)
         (= %c #xB7)
         (ccode-in-range %c #x0300 #x036F)
         (ccode-in-range %c #x203F #x2040)
         ;; &rest same as NAME-CHAR-P
         (ccode-in-range %c #xC0 #xD6)
         (ccode-in-range %c #xD8 #xF6) 
         (ccode-in-range %c #xF8 #x2FF)  
         (ccode-in-range %c #x370 #x37D)  
         (ccode-in-range %c #x37F #x1FFF)  
         (ccode-in-range %c #x200C #x200D)  
         (ccode-in-range %c #x2070 #x218F)  
         (ccode-in-range %c #x2C00 #x2FEF)  
         (ccode-in-range %c #x3001 #xD7FF)  
         (ccode-in-range %c #xF900 #xFDCF)  
         (ccode-in-range %c #xFDF0 #xFFFD)  
         (ccode-in-range %c #x10000 #xEFFFF))))

(defun nmtoken-char-p (c)
  ;; cf. http://www.w3.org/TR/REC-xml/#NT-Nmtoken
  (declare (type cdesignator c)
           (inline ccode <= =)
           (values boolean))
  (let ((%c (ccode c)))
    (declare (type character-code %c))
    (or (ncname-char-p %c)
        (= %c #.(char-code #\:)))))

    
(defun ncname-p (name)
  (declare (type rod name)
           (inline ncname-start-char-p ncname-char-p)
           (values boolean))
  (let ((len (length name)))
    (declare (type array-dimension-designator len))
    (and (not (zerop len))
         (ncname-start-char-p (rune name 0))
         (dotimes (index (1- len) t)
           (let ((c (rune-code (rune name (1+ index)))))
             (unless (ncname-char-p c)
               (return nil)))))))

;; (ncname-p "foo")
;; => T
;; (ncname-p ":foo")
;; => NIL
;; (ncname-p "xs:foo")
;; => NIL
;; (ncname-p "")
;; => NIL

(deftype ncname ()
  '(and rod (satisfies ncname-p)))

(deftype simple-ncname ()
  '(and rod (satisfies ncname-p)))

(defun nmtoken-p (name)
  (declare (type rod name)
           (inline nmtoken-char-p)
           (values boolean))
  (and (not (zerop (the array-dimension-designator
                        (length name))))
       (every #'nmtoken-char-p name)))

;; (nmtoken-p "")
;; => NIL
;; (nmtoken-p "a:b:c")
;; => T


(deftype nmtoken ()
  '(and rod (satisfies nmtoken-p)))

(deftype simple-nmtoken ()
  '(and simple-rod (satisfies nmtoken-p)))


(defun qname-p (name)
  (declare (type rod name)
           (inline ncname-start-char-p ncname-char-p)
           (values boolean))
  (let ((len (length name))
        second-ncname-p)
    (declare (type array-dimension-designator len)
             (type boolean second-ncname-p))
    (labels ((validate-ncname (offset)
               (and  (not (= offset len))
                     (ncname-start-char-p (rune name offset))
                     (dotimes (index (- len offset 1) t)
                       (let ((c (rune-code (rune name (+ index offset 1)))))
                         (unless (ncname-char-p c)
                           (return nil)))))))

    (and (not (zerop len))
         (ncname-start-char-p (rune name 0))
         (dotimes (index len t)
           (declare (type array-dimension-designator index))
           (let ((c (rune-code (rune name index))))
             (cond
               ((= c #.(char-code #\:))
                (cond
                  ((zerop index) (return nil))
                  (t ;; parse second ncname
                   (cond ((validate-ncname (1+ index))
                          (return t))
                         (t (return nil))))))
                (t (unless (ncname-char-p c)
                     (return nil))))))))))

;; (qname-p "xs:foo")
;; => T

;; (qname-p "xs:")
;; => NIL

;; (qname-p ":foo")
;; => NIL

;; (qname-p "foo")
;; => T


(deftype qname ()
  '(and rod (satisfies qname-p)))

(deftype simple-qname ()
  '(and simple-rod (satisfies qname-p)))

