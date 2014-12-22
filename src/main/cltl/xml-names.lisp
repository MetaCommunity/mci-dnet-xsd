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
    (declare (type ccode %c))
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
    (declare (type ccode %c))
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


(define-condition syntax-error (error)
  ((source
    :initarg :source
    :initform nil
    :reader syntax-error-source)
   (source-position
    :initarg :source-position
    :initform nil
    :reader syntax-error-source-position)
   (constraint
    :initarg :constraint
    :reader syntax-error-constraint))
  (:report
   (lambda (c s)
     (format s "~<Failed syntax constraint [~A]~>~
~<~@[ at position ~S~]~@[ in ~S~]~>"
             (let ((spec (syntax-error-constraint c)))
               (typecase spec
                 (function (function-name spec))
                 (t spec)))
             (syntax-error-source-position c)
             (syntax-error-source c)
             ))))


    
(defun ncname-p (name)
  ;; FIXME: Sufficient for validating a sting, 
  ;; but not sufficient for validating an input stream
  (declare (type rod name)
           (inline ncname-start-char-p ncname-char-p)
           (values boolean))
  (macrolet ((validate (form constraint &optional position)
               `(unless ,form
                  (error 'syntax-error
                         :source name
                         :constraint ,constraint
                         ,@(when position 
                                 `(:source-position ,position))))))
    (let ((len (length name)))
      (declare (type array-dimension-designator len))
      (validate (not (zerop len)) "Length not ZEROP") ;; FIXME: I18N
      (validate (ncname-start-char-p (rune name 0))
                #'ncname-start-char-p 0)
      (dotimes (index (1- len) t)
        (let ((c (rune-code (rune name (1+ index)))))
          (validate (ncname-char-p c)
                    #'ncname-char-p (1+ index)))))))

;; (ncname-p "foo")
;; => T
;; (ncname-p ":foo")
;; --> SYNTAX-ERROR
;; (ncname-p "xs:foo")
;; --> SYNTAX-ERROR
;; (ncname-p "")
;; --> SYNTAX-ERROR

(deftype ncname ()
  '(and rod (satisfies ncname-p)))

(deftype simple-ncname ()
  '(and rod (satisfies ncname-p)))

(defun nmtoken-p (name)
  (declare (type rod name)
           (inline nmtoken-char-p)
           (values boolean))
  (macrolet ((validate (form constraint &optional position)
               `(unless ,form
                  (error 'syntax-error
                         :source name
                         :constraint ,constraint
                         ,@(when position 
                                 `(:source-position ,position))))))
    (let ((len  (length name)))
    (validate (not (zerop (the array-dimension-designator
                               len)))
              "Length not ZEROP") ;; FIXME: I18N
    (dotimes (index len t)
      (let ((c (rune-code (rune name index))))
        (validate (nmtoken-char-p c)
                  #'nmtoken-char-p index))))))

;; (nmtoken-p "")
;; --> error
;;
;; (nmtoken-p "a:b:c")
;; => T
;;
;; (nmtoken-p "a!b")
;; --> error


(deftype nmtoken ()
  '(and rod (satisfies nmtoken-p)))

(deftype simple-nmtoken ()
  '(and simple-rod (satisfies nmtoken-p)))


(defun qname-p (name)
  (declare (type rod name)
           (inline ncname-start-char-p ncname-char-p)
           (values boolean))
  (macrolet ((validate (form constraint &optional position)
               `(unless ,form
                  (error 'syntax-error
                         :source name
                         :constraint ,constraint
                         ,@(when position 
                                 `(:source-position ,position))))))
    (let ((len (length name)))
      (declare (type array-dimension-designator len))
      (labels ((validate-ncname-2 (offset)
                 (validate (not (= offset len))
                           #'ncname-char-p offset)
                 (validate (ncname-start-char-p (rune name offset))
                           ncname-start-char-p offset)
                 (dotimes (index (- len offset 1) t)
                   (let* ((off (+ index offset 1))
                          (c (rune-code (rune name off))))
                     (validate (ncname-char-p c)
                               #'ncname-char-p off)))))
        (validate (not (zerop len)) "Length not ZEROP") ;; FIXME: I18N
        (validate (ncname-start-char-p (rune name 0))
                #'ncname-start-char-p 0)
        (dotimes (index len t)
          (declare (type array-dimension-designator index))
          (let ((c (rune-code (rune name index))))
            (cond
              ((= c #.(char-code #\:))
               ;; parse second ncname
               (validate-ncname-2 (1+ index)))
              (t (validate (ncname-char-p c)
                           #'ncname-char-p index)))))))))

;; (qname-p "xs:foo")
;; => T

;; (qname-p "xs:")
;; --> syntax-error

;; (qname-p ":foo")
;; --> syntax-error

;; (qname-p "foo")
;; => T


(deftype qname ()
  '(and rod (satisfies qname-p)))

(deftype simple-qname ()
  '(and simple-rod (satisfies qname-p)))


;;; % QName resolution

#+NIL ;; TO DO
(defclass namespace-context ()
  ())


#+NIL ;; TO DO
(defun resolve-qname (name)
  (declare (type qname name))
  
  )
