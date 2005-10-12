;;; Fvoxedit utilities

;; Copyright (C) 2005 Brailcom, o.p.s.

;; Author: Milan Zamazal <pdm@brailcom.org>

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.



(define-module (fvoxedit util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-6)
  :use-module (srfi srfi-13)
  :use-module (ice-9 optargs)
  :use-module (ice-9 rdelim)
  :use-module (ice-9 regex)
  :use-syntax (ice-9 syncase)
  :export (push!))


(define-public (read-from-string str)
  (let ((f (open-input-string str)))
    (read f)))

(define-public (read-from-file file-name)
  (let ((f (open-file file-name "r")))
    (read f)))

(define-public (for-file-lines file function)
  (if (access? file R_OK)
      (let ((f (open-file file "r")))
        (letrec ((next-line (lambda ()
                              (let ((line (read-line f)))
                                (if (eof-object? line)
                                    #f
                                    (or (function line)
                                        (next-line)))))))
          (next-line)))))

(define*-public (directory dir #:key pattern)
  (let ((port (opendir dir))
        (regexp (and pattern (make-regexp pattern))))
    (letrec ((entries
              (lambda (entries%)
                (let ((entry (readdir port)))
                  (if (eof-object? entry)
                      entries%
                      (entries (if (or (not pattern)
                                       (regexp-exec regexp entry))
                                   (cons (string-append dir "/" entry) entries%)
                                   entries%)))))))
      (entries '()))))

(define-public (safe-take lst n)
  (if (<= (length lst) n)
      lst
      (take lst n)))

;; `filter' is a built-in function in snd
;; taken from Guile
(define-public (filter* pred list)
  (letrec ((filiter (lambda (pred rest result)
		      (if (null? rest)
			  (reverse! result)
			  (filiter pred (cdr rest)
				   (cond ((pred (car rest))
					  (cons (car rest) result))
					 (else
					  result)))))))
    (filiter pred list '())))

(define-syntax push! (syntax-rules ()
                       ((_ obj list-var)
                        (set! list-var (cons obj list-var)))))
