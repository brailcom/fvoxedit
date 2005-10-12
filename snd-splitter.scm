;;; Splitting recording to parts separated by silences

;; Copyright (C) 2003, 2005 Brailcom, o.p.s.

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



(define-module (fvoxedit snd-splitter)
  :use-module (srfi srfi-1)
  :use-module (ice-9 format)
  :use-module (guile-user)              ; snd functions
  :export (*noise-level* *chunk-length* *min-silence-length*
           *file-name-format*))


;;; Configuration variables and functions


(define *noise-level* 0.02)             ; 0.0 .. 1.0

(define *chunk-length* 0.005)           ; seconds

(define *min-silence-length* 0.5)       ; seconds

(define *file-name-format* "~4,'0D")


(define (handle-silence beg end)
  (add-mark (inexact->exact (/ (+ beg end) 2))))


;;; Internal functions


(define (seconds->samples* secs)
  (* 2 (seconds->samples secs)))

(define (samples->seconds* secs)
  (/ (samples->seconds secs) 2))

(define (next-chunk-value reader)
  (let ((nsamples (seconds->samples* *chunk-length*))
        (i 0)
        (sum 0))
    (while (and (< i nsamples) (not (sample-reader-at-end? reader)))
      (set! sum (+ sum (abs (next-sample reader))))
      (set! i (+ i 1)))
    (if (sample-reader-at-end? reader)
        #f
        (/ sum i))))

(define (next-chunk-state reader)
  (let ((chunk-value (next-chunk-value reader)))
    (cond
     ((or (not chunk-value) (c-g?))
      #f)
     ((> chunk-value *noise-level*)
      'sound)
     (else
      'silence))))

(define (skip-silence reader)
  (let ((position (sample-reader-position reader)))
    (case (next-chunk-state reader)
      ((silence)
       (skip-silence reader))
      ((sound)
       position)
      (else
       #f))))

(define (next-silence reader)
  (let ((beg (sample-reader-position reader))
        (end (skip-silence reader)))
    (cond
     ((not end)
      #f)
     ((>= (- (samples->seconds* end) (samples->seconds* beg))
          *min-silence-length*)
      (list beg end))
     (else
      (next-silence reader)))))

(define (find-process-silences)
  (let ((reader (make-sample-reader 0)))
    (letrec ((process (lambda ()
                        (let ((silence (next-silence reader)))
                          (if silence
                              (begin
                                (apply handle-silence silence)
                                (process)))))))
      (skip-silence reader)
      (process))))


;;; User interface


(define (com-find-silences)
  (delete-marks)
  (find-process-silences)
  #f)

(define (com-save-parts)
  (let ((i 1)
        (marks* (map mark-sample (caar (marks)))))
    (for-each
     (lambda (beg end)
       (let ((region (make-region beg end (selected-sound) #t)))
         (save-region region :file (format #f *file-name-format* i)
                      :data-format (data-format) :header-type (header-type))
         (forget-region region)
         (set! i (+ i 1))))
     (cons 0 marks*) (append marks* (list (frames)))))
  (set! (selection-member?) #f)
  #f)

(define (com-adjust-parameters)
  (report-in-minibuffer "Not yet implemented.  Set guile variables instead.")
  #f)

(if (not (provided? 'splitter))
    (let ((m (add-to-main-menu "Splitter")))
      (add-to-menu m "Identify silences" com-find-silences)
      (add-to-menu m "Save parts" com-save-parts)))
