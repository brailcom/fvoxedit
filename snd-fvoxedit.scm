;;; Editing Festival diphone definitions in snd

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
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.


(define-module (fvoxedit snd-fvoxedit)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-13)
  :use-module (ice-9 regex)
  :use-module (guile-user)              ; snd functions
  :use-module (fvoxedit festival)
  :use-module (fvoxedit util))


;;; Configuration


(define-public *show-pitchmarks* #t)

(define-public *show-labels* #f)

(define-public *mouse-step* 40)

(define-public *max-displayed-candidates* 8)


;;; Functions


(define (current-diphone-marks)
  (filter* diphone-mark? (all-marks)))

(define (current-diphone)
  (sound-property 'diphone (selected-sound)))

(define (diphone-mark? mark)
  (member (mark-name mark) '("beg" "mid" "end")))

(define (pitch-mark? mark)
  (not (named-mark? mark)))

(define* (current-pitchmarks #:optional (snd (selected-sound))
                             (chn (selected-channel)))
  (filter* pitch-mark? (marks snd chn)))

(define* (current-labels #:optional (snd (selected-sound))
                         (chn (selected-channel)))
  (filter* phone-mark? (marks snd chn)))

(define +phone-mark-prefix+ "<=")

(define (mark-phone mark)
  (let ((name (mark-name mark)))
    (if (and (not (equal? name ""))
             (equal? (substring name 0 (string-length +phone-mark-prefix+))
                     +phone-mark-prefix+))
        (substring name (string-length +phone-mark-prefix+))
        #f)))

(define phone-mark? mark-phone)

(define (refresh-window)
  (let ((w (list-ref (main-widgets) 2)))
    (hide-widget w)
    (show-widget w)))

(define boundary-mark-color (make-color 1 0 0))

(define pitchmark-color (make-color 0 0 1))

(define label-mark-color (make-color 0 0.7 0))

(define (named-mark? mark)
  (not (equal? (mark-name mark) "")))

(define (fvoxedit-draw-mark-hook id)
  (cond
   ((and (not *show-pitchmarks*)
         (not (named-mark? id)))
    #t)
   ((and *show-labels*
         (not (diphone-mark? id))
         (phone-mark? id))
    (if (not (equal? (mark-color) label-mark-color))
        (set! (mark-color) label-mark-color))
    #f)
   ((named-mark? id)
    (if (phone-mark? id)
        #t
        (begin
          (if (not (equal? (mark-color) boundary-mark-color))
              (set! (mark-color) boundary-mark-color))
          #f)))
   (else
    (if (not (equal? (mark-color) pitchmark-color))
        (set! (mark-color) pitchmark-color))
    #f)))

(if (not (provided? 'fvoxedit))
    (add-hook! draw-mark-hook fvoxedit-draw-mark-hook))

(define (seconds->samples* secs)
  (* 2 (seconds->samples secs)))

(define (samples->seconds* secs)
  (/ (samples->seconds secs) 2))

(define (prompt prompt function)
  (or (selected-sound) (new-sound))
  (report-in-minibuffer "")
  (prompt-in-minibuffer prompt function (selected-sound) #t))

(define (add-named-mark time name)
  (let ((m (add-mark (min (seconds->samples* time) (- (frames) 1)))))
    (set! (mark-name m) name)
    m))

(define (add-boundary-marks diphone)
  (if (null? (diphone-boundaries diphone))
      #f
      (begin
        (add-named-mark (diphone-beg diphone) "beg")
        (add-named-mark (diphone-end diphone) "end")
        (add-named-mark (diphone-mid diphone) "mid")
        (set! (sound-property 'boundaries-changed (selected-sound)) #f))))

(define (add-pitchmarks diphone)
  (for-each add-mark (map seconds->samples* (diphone-pitchmarks diphone)))
  (set! (sound-property 'pitchmarks-changed (selected-sound)) #f))

(define (add-label-marks diphone)
  (for-each (lambda (label)
              (add-named-mark
               (label-time label) 
               (string-append +phone-mark-prefix+ (label-name label))))
            (diphone-labels diphone))
  (set! (sound-property 'labels-changed (selected-sound)) #f))

(define* (show-given-diphone diphone #:optional name)
  (let ((name (or name (diphone-name diphone)))
        (snd (view-sound (diphone-wav-file diphone))))
    (set! (sound-property 'diphone snd) diphone))
  ;; There seems to be a bug in snd's x-axis-label setter
  (catch 'wrong-type-arg
         (lambda ()
           (set! (x-axis-label)
                 (if (equal? name (diphone-name diphone))
                     name
                     (format #f "~A (~A)"
                             (diphone-name diphone)
                             name))))
         (lambda (arg . rest) #f))
  (if *show-pitchmarks*
      (add-pitchmarks diphone))
  (if *show-labels*
      (add-label-marks diphone))
  (let ((factor (diphone-power-factor diphone)))
    (set! (y-zoom-slider) factor)
    (set! (y-position-slider) (/ (- 1 factor) 2)))
  (let ((zoom (if (and *show-pitchmarks*
                       (not (null? (diphone-boundaries diphone))))
                  (min (/ (+ 0.1 (- (diphone-end diphone) (diphone-beg diphone)))
                          (samples->seconds* (frames)))
                       1)
                  1))
        (mid-mark (add-boundary-marks diphone)))
    (set! (x-zoom-slider) zoom)
    (if mid-mark
        (set! (cursor) (mark-sample mid-mark)))
    (if (not (= zoom 1))
        (set! (x-position-slider) (/ (- (diphone-beg diphone) 0.05)
                                     (samples->seconds* (frames))))))
  cursor-in-middle)

(define* (show-diphone name #:optional (all? #f))
  (let ((diphones (get-diphone name)))
    (if (null? diphones)
        (snd-error (format #f "No such diphone: ~a" name))
        (if all?
            (for-each (lambda (d) (show-given-diphone d name)) diphones)
            (begin
              (if (selected-sound)
                  (close-sound (selected-sound)))
              (show-given-diphone (car diphones) name)
              (play))))))

(define (show-all-diphones name)
  (show-diphone name #t))

(define (show-all-diphone-candidates name)
  (for-each show-given-diphone (safe-take (diphone-candidates name)
                                          *max-displayed-candidates*)))

(define (play-between-marks m1 m2)
  (play-region (make-region (if (eq? m1 #f) 0 (mark-sample m1))
                            (min (mark-sample m2) (- (frames) 1)))
               #t))

(define (play-between-named-marks name1 name2)
  (let ((mark1 (find-mark name1))
        (mark2 (find-mark name2)))
    (if (or (not mark1) (not mark2))
        (snd-error "Marks not found.")
        (play-between-marks mark1 mark2))))

(define (move-mark mark sample)
  (if *show-labels*
      (let ((mid-mark (find-mark "mid")))
        (cond
         ((and mid-mark
               (phone-mark? mark)
               (= (mark-sample mark) (mark-sample mid-mark)))
          (set! (mark-sample mid-mark) sample))
         ((equal? mark mid-mark)
          (let ((m (nearest-mark (phone-marks) (mark-sample mid-mark))))
            (if m
                (set! (mark-sample m) sample)))))))
  (set! (mark-sample mark) sample))

(define* (move-nearest-mark snd chn x axis marks #:optional direction shift)
  (if (not (null? marks))
      (let* ((axinfo (axis-info snd chn axis))
             (x0 (list-ref axinfo 10))
             (x1 (list-ref axinfo 12))
             (sample0 (list-ref axinfo 0))
             (sample1 (list-ref axinfo 1))
             (click-sample (if (eq? x #t)
                               (cursor)
                               (+ sample0
                                  (inexact->exact (* (/ (- x x0) (- x1 x0))
                                                     (- sample1 sample0))))))
             (m (if (eq? direction #f)
                    (nearest-mark marks click-sample)
                    (or ((if (eq? direction 'left) car cadr)
                         (surrounding-marks marks click-sample))
                        (car marks))))
             (sample (if shift
                         (+ (mark-sample m)
                            (if (pitch-mark? m) (if (> shift 0) 1 -1) shift))
                         click-sample)))
        (move-mark m sample))))

(define (center-diphone-mark)
  (let* ((sample (cursor))
         (mark (find-mark (if (< sample (mark-sample (find-mark "mid")))
                              "beg"
                              "end")))
         (labels (surrounding-marks (phone-marks))))
    (if mark
        (set! (mark-sample mark)
              (inexact->exact (/ (apply + (map mark-sample labels)) 2))))))

(define (all-marks)
  (marks (selected-sound) (selected-channel)))

(define (all-visible-marks)
  (filter* (lambda (m)
             (or (and *show-labels* (phone-mark? m))
                 (and *show-pitchmarks* (pitch-mark? m))
                 (diphone-mark? m)))
           (all-marks)))

(define (phone-marks)
  (filter* phone-mark? (all-marks)))

(define* (surrounding-marks marks% #:optional (sample (cursor)))
  (set! marks% (cons #f marks%))
  (while (and (not (null? marks%)) (not (null? (cdr marks%)))
              (> sample (mark-sample (cadr marks%))))
    (set! marks% (cdr marks%)))
  (list (if (null? marks%) #f (car marks%))
        (if (< (length marks%) 2) #f (cadr marks%))))

(define* (nearest-mark marks% #:optional (sample (cursor)))
  (let* ((ms (surrounding-marks marks% sample))
         (m1 (car ms))
         (m2 (cadr ms)))
    (cond
     ((not m1)
      m2)
     ((not m2)
      m1)
     ((< (abs (- sample (mark-sample m1))) (abs (- sample (mark-sample m2))))
      m1)
     (else
      m2))))

(define (fvoxedit-mark-hook id snd chn reason)
  (cond
   ((diphone-mark? id)
    (set! (sound-property 'boundaries-changed snd) #t))
   ((pitch-mark? id)
    (set! (sound-property 'pitchmarks-changed snd) #t))
   ((phone-mark? id)
    (set! (sound-property 'labels-changed snd) #t))))
(add-hook! mark-hook fvoxedit-mark-hook)

(define (synthesize synth-func)
  (let ((labels (diphone-labels (current-diphone))))
    (if (null? labels)
        (snd-error "No labels available")
        (synth-func labels))))


;;; Commands


(define (com-show-diphone)
  (prompt "diphone:" show-diphone))

(define (com-show-all-diphones)
  (prompt "diphone:" show-all-diphones))

(define (com-show-all-diphone-candidates)
  (prompt "diphone" show-all-diphone-candidates))

(define (com-next-diphone)
  (let ((diphone (current-diphone)))
    (if (not diphone)
        (snd-error "No diphone loaded."))
    (let* ((name (diphone-true-name diphone))
           (match (string-match "^(|.*[^0-9])([0-9]+)$" name)))
      (if match
          (let* ((number-string (match:substring match 2))
                 (number-len (string-length number-string))
                 (number (1+ (read-from-string number-string)))
                 (new-number (format #f "~D" number)))
            (while (< (string-length new-number) number-len)
              (set! new-number (string-append "0" new-number)))
            (show-diphone new-number)))))
  keyboard-no-action)

(define (com-toggle-pitchmarks)
  (set! *show-pitchmarks* (not *show-pitchmarks*))
  (if (and *show-pitchmarks*
           (null? (current-pitchmarks)))
      (add-pitchmarks (current-diphone)))
  (refresh-window)
  keyboard-no-action)

(define (com-toggle-labels)
  (set! *show-labels* (not *show-labels*))
  (if (and *show-labels*
           (null? (current-labels)))
      (add-label-marks (current-diphone)))
  (refresh-window)
  keyboard-no-action)

(define (com-play-diphone)
  (play-between-named-marks "beg" "end")
  keyboard-no-action)

(define (com-play-first)
  (play-between-named-marks "beg" "mid")
  keyboard-no-action)

(define (com-play-second)
  (play-between-named-marks "mid" "end")
  keyboard-no-action)

(define (com-play-phones)
  (let ((marks* (phone-marks)))
    (for-each (lambda (name1 name2)
                (play-between-marks name1 name2)
                (usleep 500000))
              (cons #f (drop-right marks* 1))
              marks*))
  keyboard-no-action)

(define (com-play-current-phone)
  (let ((marks% (phone-marks)))
    (if (null? marks%)
        (report-in-minibuffer "No labels present.")
        (let ((ms (surrounding-marks marks%)))
          (apply play-between-marks ms)
          (report-in-minibuffer (or (mark-name (cadr ms)) "$")))))
  keyboard-no-action)

(define (save default?)
  (let ((diphone (current-diphone)))
    (if diphone
        (let ((marks->seconds (lambda (marks%)
                                (map (lambda (m)
                                       (samples->seconds* (mark-sample m)))
                                     marks%)))
              (snd (selected-sound)))
          ;; diphones
          (if (or default? (sound-property 'boundaries-changed snd))
              (let ((diphone-marks (current-diphone-marks)))
                (if (not (null? diphone-marks))
                    (begin
                      (apply set-diphone-boundaries diphone
                             (marks->seconds diphone-marks))
                      (save-diphone diphone default?)))
                (set! (sound-property 'boundaries-changed snd) #f)))
          ;; pitchmarks
          (if (sound-property 'pitchmarks-changed snd)
              (begin
                (save-pitchmarks diphone (marks->seconds (current-pitchmarks)))
                (set! (sound-property 'pitchmarks-changed snd) #f)))
          ;; labels
          (if (sound-property 'labels-changed snd)
              (begin
                (save-labels diphone
                             (map (lambda (m)
                                    (make-label
                                     (substring
                                      (mark-name m)
                                      (string-length +phone-mark-prefix+))
                                     (samples->seconds* (mark-sample m))))
                                  (current-labels)))
                (set! (sound-property 'labels-changed snd) #f)))
          (report-in-minibuffer "Saved."))))
  keyboard-no-action)

(define (com-save)
  (save #f))

(define (com-save-default)
  (save #t))

(define (com-align-midmark)
  (let ((phone-mark (nearest-mark (phone-marks))))
    (if phone-mark
        (set! (mark-sample (find-mark "mid")) (mark-sample phone-mark))))
  keyboard-no-action)

(define (com-center-diphone-mark)
  (center-diphone-mark)
  keyboard-no-action)

(define (com-place-initial-silence)
  (let ((labels (current-labels)))
    (if (>= (length labels) 2)
        (set! (mark-sample (car labels))
              (max 0 (- (mark-sample (cadr labels)) (seconds->samples* 0.1))))))
  keyboard-no-action)

(define (com-synthesize)
  (synthesize synthesize-labels)
  keyboard-no-action)

(define (com-synthesize-show)
  (view-sound (synthesize synthesize-labels-save))
  (set! (x-zoom-slider) 1)
  keyboard-no-action)

(define (com-mouse-mark snd chn button state x y axis)
  (if (and (= axis time-graph)
           (not (eq? (sound-property 'diphone snd) #f)))
      (cond
       ((= button 2)
        (cond
         (*show-pitchmarks*
          (move-nearest-mark snd chn x axis (current-pitchmarks))
          #t)
         (*show-labels*
          (move-nearest-mark snd chn x axis (current-labels))
          #t)
         (else
          #f)))
       ((= button 3)
        (move-nearest-mark snd chn x axis (current-diphone-marks))
        #t)
       ((= button 4)
        (move-nearest-mark snd chn x axis (all-visible-marks)
                           #f (- *mouse-step*))
        #t)
       ((= button 5)
        (move-nearest-mark snd chn x axis (all-visible-marks)
                           #f *mouse-step*)
        #t)
       ((= button 6)
        (move-nearest-mark snd chn x axis (all-visible-marks) 'left)
        #t)
       ((= button 7)
        (move-nearest-mark snd chn x axis (all-visible-marks) 'right)
        #t)
       (else
        #f))
      #f))


;;; Keys


(define (bind-key* key command)
  (bind-key (char->integer key) 0 command))

(bind-key* #\a com-align-midmark)
(bind-key* #\c com-center-diphone-mark)
(bind-key* #\C com-show-all-diphone-candidates)
(bind-key* #\d com-show-diphone)
(bind-key* #\D com-show-all-diphones)
(bind-key* #\i com-place-initial-silence)
(bind-key* #\l com-toggle-labels)
(bind-key* #\n com-next-diphone)
(bind-key* #\p com-toggle-pitchmarks)
(bind-key* #\s com-save)
(bind-key* #\S com-save-default)
(bind-key* #\t com-synthesize)
(bind-key* #\T com-synthesize-show)
(bind-key* #\1 com-play-first)
(bind-key* #\2 com-play-second)
(bind-key* #\3 com-play-diphone)
(bind-key* #\4 com-play-phones)
(bind-key* #\5 com-play-current-phone)

(add-hook! mouse-click-hook com-mouse-mark)


;;; Menus


(if (not (provided? 'fvoxedit))
    (let ((m (add-to-main-menu "Fvoxedit")))
      (add-to-menu m "Load diphone" com-show-diphone)
      (add-to-menu m "Load all diphones" com-show-all-diphones)
      (add-to-menu m "Load all diphone candidates" com-show-all-diphone-candidates)
      (add-to-menu m "Toggle pitchmarks" com-toggle-pitchmarks)
      (add-to-menu m "Toggle labels" com-toggle-labels)
      (add-to-menu m "Save changes" com-save)
      (add-to-menu m "Save and make diphone default" com-save-default)
      (add-to-menu m "Play diphone" com-play-diphone)
      (add-to-menu m "Play first phone" com-play-first)
      (add-to-menu m "Play second phone" com-play-second)
      (add-to-menu m "Play phones" com-play-phones)
      (add-to-menu m "Play cursor phone" com-play-current-phone)
      (add-to-menu m "Align diphone mid mark with label" com-align-midmark)
      (add-to-menu m "Center diphone boundary mark" com-center-diphone-mark)
      (add-to-menu m "Place initial silence" com-place-initial-silence)
      (add-to-menu m "Load next diphone" com-next-diphone)
      (add-to-menu m "Synthesize sample" com-synthesize)
      (add-to-menu m "Synthesize and view sample" com-synthesize-show)
      ))
