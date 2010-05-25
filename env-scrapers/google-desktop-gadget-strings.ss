#!/usr/bin/env mzscheme
#lang scheme
; Creates a type environment from an XML strings source. It just looks
; in the XML for a tag called STRINGS, then outputs all sub-tags as
; STRINGS, e.g.
; <strings><GADGET_NAME>blahblah</GADGET_NAME></strings>
; will output
; GADGET_NAME : Str
;
; This tool does not raise an error if a name a used multiple times.
(require xml)

(define head (document-element (read-xml)))

(define (extract-name e)
  (if (element? e)
      (string-append (symbol->string (element-name e)) " : Str")
      5))

(if (not (symbol=? (element-name head) 'strings))
    (error "head element must be called <strings>")
    (begin
      (let ((els (element-content head)))
       (let ((names (filter string? (map extract-name els))))
         (begin
           (printf (apply string-append (add-between names "~n")))
           (printf "~nstrings : {~n  ")
           (printf (apply string-append (add-between names ",~n  ")))
           (printf "~n}~n"))))))
