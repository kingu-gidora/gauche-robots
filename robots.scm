;;;
;;; robots.scm - robots.txt parser (v0.0.1)
;;;
;;; Copyright (c) 2010 Takuya Mannami <mtakuya@users.sourceforge.jp>
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;;
;;; 3. Neither the name of the authors nor the names of its contributors
;;; may be used to endorse or promote products derived from this
;;; software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
 
(define-module robots
  (use util.list)
  (export parse-robots robots->dump))
(select-module robots)

(define (parse-robots txt)
  (let ((ua-hash  (make-hash-table 'string=?))
        (txt-list (string-split txt #\newline))
        (_ua #f)
        (sitem-list '())
        (USER-AGENT "User-agent")
        (SITE-MAP   "Sitemap"))
    (for-each 
     (lambda (txt)
       (if-let1 match (#/^(\w.*):\s(.*)/ txt)
                (let ((_key (match 1)) (_val (match 2)))
                      (cond ((string=? _key USER-AGENT)
                             (if (hash-table-exists? ua-hash _val)
                                 (set! _ua _val)
                                 (begin
                                   (hash-table-put! ua-hash _val (make-hash-table 'string=?))
                                   (set! _ua _val))))
                            ((string=? _key SITE-MAP)
                             (set! sitem-list (append `(,_val) sitem-list)))
                            (else
                             (let1 _ht (ref ua-hash _ua)
                               (if (hash-table-exists? _ht _key)
                                   (hash-table-update! _ht _key (cut append `(,_val) <>))
                                   (hash-table-put! _ht _key `(,_val)))))))))
    txt-list)
  (values ua-hash sitem-list)))

(define-syntax robots->dump
  (syntax-rules ()
    ((_ robots)
     (map (lambda (hl)
            (list (car hl)
                  (hash-table->alist (cdr hl))))
          (hash-table->alist robots)))
    ((_ robots '(ua str)) (ref (ref robots ua) str))))

(provide "robots")