#!/usr/bin/env mzscheme
#lang scheme
; Creates a type environment from an XML source. The type environment binds
; each named element to the element's name. For example, if the file contains:
; <img name="foo">
; we add foo : img to the environment.
;
; This tool raises an error if a name a used multiple times.
(require xml)

(define ((attr-name=? name) attr)
  (symbol=? (attribute-name attr) name))

(define (content->bind elt)
  (make-immutable-hash
   (if (element? elt)
       (let ([name-attr (findf (attr-name=? 'name) (element-attributes elt))])
         (if name-attr
             (list (cons (attribute-value name-attr) (element-name elt)))
             empty))
       empty)))

(define (xml-mapQ f v)
  (if (element? v)
      (map f (element-content v))
      empty))

(define (xml-everywhere combine f v)
  (foldr combine (f v)
         (xml-mapQ (lambda (w) (xml-everywhere combine f w)) v)))

(define (combine-hash h1 h2)
  (for/fold ([hash h1])
    ([(k v) (in-hash h2)])
    (if (hash-has-key? hash k)
        (error 'combine-hash "duplicate name: ~a" k)
        (hash-set hash k v))))

(define (xml->binds xml)
  (xml-everywhere combine-hash content->bind xml))

(define (string-capitalize s)
  (string-append (string-upcase (substring s 0 1)) (string-downcase (substring s 1))))

(define (print-env env)
  (for ([(x t) (in-hash env)])
    (printf "~a : ~a~n" x (string-capitalize (symbol->string t)))))

(print-env (xml->binds (document-element (read-xml))))
