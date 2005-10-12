;;; snd function exports

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


(use-modules (srfi srfi-1))
(set! %load-path (lset-adjoin equal? %load-path "/usr/share/snd"))
(load-from-path "extensions")
(load-from-path "marks")

(export add-mark)
(export add-to-main-menu)
(export add-to-menu)
(export axis-info)
(export bind-key)
(export close-sound)
(export c-g?)
(export cursor)
(export cursor-in-middle)
(export data-format)
(re-export define*)
(export delete-mark)
(export delete-marks)
(export draw-mark-hook)
(export find-mark)
(export forget-region)
(export frames)
(export header-type)
(export hide-widget)
(export keyboard-no-action)
(re-export lambda*)
(re-export let-keywords*)
(re-export let-optional*)
(export main-widgets)
(export make-color)
(export make-region)
(export make-sample-reader)
(export mark-color)
(export mark-hook)
(export mark-name)
(export mark-name->id)
(export mark-sample)
(export marks)
(export mouse-click-hook)
(export new-sound)
(export next-sample)
(export play)
(export play-region)
(export prompt-in-minibuffer)
(export report-in-minibuffer)
(export sample-reader-at-end?)
(export sample-reader-position)
(export samples->seconds)
(export save-region)
(export seconds->samples)
(export selected-channel)
(export selected-sound)
(export selection-member?)
(export show-widget)
(export snd-error)
(export sound-property)
(export time-graph)
(export view-sound)
(export x-axis-label)
(export x-position-slider)
(export x-zoom-slider)
(export y-position-slider)
(export y-zoom-slider)

(provide 'snd-exports)
