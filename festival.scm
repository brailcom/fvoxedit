;;; Festival related functions

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


(define-module (fvoxedit festival)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-13)
  :use-module (ice-9 format)
  :use-module (ice-9 optargs)
  :use-module (ice-9 regex)
  :use-module (fvoxedit util))


;;; Configuration


(define-public *voice-directory* ".")

(define-public *diphone-dic-file* "dic/diph.est")

(define-public *diphone-wav-dir* "wav")

(define-public *diphone-pm-dir* "pm")

(define-public *diphone-lab-dir* "lab")

(define-public *powfacts-file* "etc/powfacts")

(define-public *powfacts-dir* "etc/pf")

(define-public *tmp-sound-file* #f)


;;; Utility functions


(define (subdirectory path)
  (if (string-prefix? "/" path)
      path
      (string-append *voice-directory* "/" path)))


;;; Diphones


(define* (diphone-entry-regexp diphone-name #:optional (file-name #f))
  (make-regexp (format #f "^(~a) +(~a) +([^ ]+) +([^ ]+) +([^ ]+)"
                       (if diphone-name
                           (regexp-quote diphone-name)
                           "[^ ]+")
                       (if file-name
                           (format #f "[^ ]*~a" (regexp-quote file-name))
                           "[^ ]+"))))

(define-public (get-diphone name)
  (let* ((file-name? (string-match "^[0-9]+$" name))
         (rx (if file-name?
                 (diphone-entry-regexp #f name)
                 (diphone-entry-regexp name)))
         (alias-rx (and (not file-name?)
                        (make-regexp
                         (format #f "^~a +&([^ ]+)" (regexp-quote name)))))
         (diphones '()))
    (for-file-lines (subdirectory *diphone-dic-file*)
      (lambda (line)
        (let ((match (regexp-exec rx line)))
          (if match
              (begin
                (push! (list (match:substring match 1)
                             (match:substring match 2)
                             (read-from-string (match:substring match 3))
                             (read-from-string (match:substring match 4))
                             (read-from-string (match:substring match 5)))
                       diphones)
                #f)
              (let ((match (and alias-rx (regexp-exec alias-rx line))))
                (if (and match
                         (not diphones))
                    (set! diphones (get-diphone (match:substring match 1)))
                    #f))))))
    (if (and (null? diphones)
             file-name?)
        (for-each (lambda (f)
                    (push! (list #f (basename f ".wav") #f #f #f) diphones))
                  (directory (subdirectory *diphone-wav-dir*)
                             #:pattern (string-append name ".*\\.wav$"))))
    diphones))

(define (file-diphone-name file-name)
  (match:substring (string-match "([0-9]+).*\\.(lab|pm|wav)$" file-name) 1))

(define-public (diphone-candidates name)
  (let* ((result '())
         (name-match (string-match "^(.*)-(.*)$" name))
         (phone-pair (cons (match:substring name-match 1) (match:substring name-match 2))))
    (for-each (lambda (file-name)
                (let ((phones (map label-name (file-labels file-name))))
                  (if (member phone-pair (map cons (drop-right phones 1) (drop phones 1)))
                      (push! (car (get-diphone (file-diphone-name file-name))) result))))
              (directory (subdirectory *diphone-lab-dir*) #:pattern "\\.lab$"))
    (reverse result)))

(define-public (diphone-name diphone)
  (list-ref diphone 0))

(define-public (diphone-true-name diphone)
  (list-ref diphone 1))

(define-public (diphone-beg diphone)
  (list-ref diphone 2))

(define-public (diphone-mid diphone)
  (list-ref diphone 3))

(define-public (diphone-end diphone)
  (list-ref diphone 4))

(define-public (diphone-boundaries diphone)
  (if (diphone-beg diphone)
      (list (diphone-beg diphone) (diphone-mid diphone) (diphone-end diphone))
      '()))

(define-public (set-diphone-boundaries diphone beg mid end)
  (if (not (< beg mid end))
      (error "Invalid diphone boundaries" beg mid end))
  (list-set! diphone 2 beg)
  (list-set! diphone 3 mid)
  (list-set! diphone 4 end))

(define-public (diphone-wav-file diphone)
  (string-append (subdirectory *diphone-wav-dir*)
                 "/" (diphone-true-name diphone) ".wav"))

(define-public (save-diphone diphone default?)
  (let* ((name (diphone-name diphone))
         (true-name (diphone-true-name diphone))
         (beg (diphone-beg diphone))
         (mid (diphone-mid diphone))
         (end (diphone-end diphone))
         (index-file (subdirectory *diphone-dic-file*))
         (pathname (string-append index-file ".tmp"))
         (port (open-file pathname "w"))
         (rx (diphone-entry-regexp name true-name))
         (output-diphone (lambda ()
                           (format port "~A ~A ~,3F ~,3F ~,3F~%"
                                   name true-name beg mid end))))
    (for-file-lines index-file
      (lambda (line)
        (let ((match (regexp-exec rx line)))
          (if match
              (if (not default?)
                  (output-diphone))
              (format port "~A~%" line)))
        #f))
    (if default?
        (output-diphone))
    (close-port port)
    (rename-file pathname index-file)))


;;; Pitchmarks


(define (diphone-pitchmark-file diphone)
  (string-append (subdirectory *diphone-pm-dir*)
                 "/" (diphone-true-name diphone) ".pm"))

(define-public (diphone-pitchmarks diphone)
  (let ((rx (make-regexp "^([0-9]+\.?[0-9]*)"))
        (pitchmarks '())
        (pitchmark-file (diphone-pitchmark-file diphone)))
    (if (access? pitchmark-file R_OK)
        (for-file-lines pitchmark-file
          (lambda (line)
            (let ((match (regexp-exec rx line)))
              (if match
                  (set! pitchmarks (cons
                                    (read-from-string (match:substring match 1))
                                    pitchmarks))))
            #f)))
    (reverse! pitchmarks)))

(define +pitchmark-file-header+ "EST_File Track
DataType ascii
NumFrames ~D
NumChannels 0
NumAuxChannels 0
EqualSpace 0
BreaksPresent true
file_type 13
EST_Header_End
")  
(define-public (save-pitchmarks diphone pitchmarks)
  (if (not (null? pitchmarks))
      (let* ((pmfile (diphone-pitchmark-file diphone))
             (pathname (string-append pmfile ".tmp"))
             (port (open-file pathname "w")))
        (format port +pitchmark-file-header+ (length pitchmarks))
        (for-each (lambda (m) (format port "~F    1~%" m)) pitchmarks)
        (close-port port)
        (rename-file pathname pmfile))))


;;; Labeling


(define (diphone-label-file diphone)
  (string-append (subdirectory *diphone-lab-dir*)
                 "/" (diphone-true-name diphone) ".lab"))

(define-public (make-label name time)
  (list name time))

(define-public (label-name label)
  (list-ref label 0))

(define-public (label-time label)
  (list-ref label 1))

(define-public (file-labels file-name)
  (let ((rx (make-regexp "^[ \t]+([0-9]+\.?[0-9]*)[ \t]+[0-9]+[ \t]+(.+)$"))
        (labels '()))
    (if (access? file-name R_OK)
        (for-file-lines file-name
          (lambda (line)
            (let ((match (regexp-exec rx line)))
              (if match
                  (push! (make-label (match:substring match 2)
                                     (read-from-string
                                      (match:substring match 1)))
                         labels)))
            #f)))
    (reverse! labels)))
                
(define-public (diphone-labels diphone)
  (file-labels (diphone-label-file diphone)))

(define +label-file-header+ "separator ;
nfields 1
#
")
(define-public (save-labels diphone labels)
  (if (not (null? labels))
      (let* ((lab-file (diphone-label-file diphone))
             (pathname (string-append lab-file ".tmp"))
             (port (open-file pathname "w")))
        (format port +label-file-header+ (length labels))
        (for-each (lambda (l)
                    (format port "\t ~,3F 26 ~A~%"
                            (label-time l) (label-name l)))
                  labels)
        (close-port port)
        (rename-file pathname lab-file))))

(define-public (synthesize-labels labels)
  (system (format #f "echo '(SayPhones (quote (~{~A ~})))' | festival --pipe"
                  (map label-name labels))))

(define-public (synthesize-labels-save labels)
  (let ((tmpfile (or *tmp-sound-file*
                     (begin
                       (set! *tmp-sound-file*
                             (port-filename
                              (mkstemp! (string-append
                                         (or (getenv "TMPDIR") "/tmp")
                                         "/fvoxeditXXXXXX"))))
                       *tmp-sound-file*))))
    (system (format #f "echo '(utt.save.wave (SayPhones (quote (~{~A ~}))) \"~A\")' | festival --pipe"
                    (map label-name labels) tmpfile))
    tmpfile))


;;; Power factors


(define-public (diphone-power-factor diphone)
  (let* ((min-factor 1)
         (true-name (diphone-true-name diphone))
         (factor-file (subdirectory
                       (string-append *powfacts-dir* "/" true-name)))
         (diphone-factor (if (access? factor-file R_OK)
                             (read-from-file factor-file)
                             #f))
         (diphone-file-name true-name)
         (regexp (make-regexp "^([^ ]+) ([0-9.]+)")))
    (for-file-lines (subdirectory *powfacts-file*)
      (lambda (line)
        (let* ((match (regexp-exec regexp line))
               (factor (read-from-string (match:substring match 2))))
          (if (and (not diphone-factor)
                   (string= (match:substring match 1) diphone-file-name))
              (set! diphone-factor factor))
          (if (< factor min-factor)
              (set! min-factor factor))
          #f)))
    (/ min-factor (or diphone-factor 1))))
