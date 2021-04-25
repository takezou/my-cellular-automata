;;; my-cellular-automata.el --- My Cellular Automata  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Takeshi Fujiwara

;; Author: Takeshi Fujiwara <156400+takezou@users.noreply.github.com>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Play cellular automata.
;; Puts a node at point in the feld by hitting space key.
;; Update the field by "u" key. C-u <numer> u to do updates <number> times at once.
;; Set *my-cellular-automata-test-mode* to non-nil to prepopulate some nodes.
;; Set *my-cellular-automata-field-size* to modify the field width and height.
;;
;;; Code:

;; evaluate to test overriding defaults:
;; (setq *my-cellular-automata-test-mode* t)
;; (setq *my-cellular-automata-node-char* ?\🐯)
;; (setq *my-cellular-automata-blank-char* ?\🌀)

(defvar *my-cellular-automata-test-mode* nil
  "set non-nil to turn on test mode")

(defvar *my-cellular-automata-buffer-name* "*my-cellular-automata*"
  "buffer name")

(defvar *my-cellular-automata-timer* nil
  "timer for periodical update")

(defun my-cellular-automata ()
  "play cellular automata"
  (interactive)
  (my-cellular-automata-init)
  (if *my-cellular-automata-test-mode* (my-cellular-automata-put-test-nodes))
  (switch-to-buffer *my-cellular-automata-buffer-name*)
  (my-cellular-automata-mode)
  (my-cellular-automata-show-field)
  )

(defun my-cellular-automata-put-test-nodes ()
  "init for testing"
  (dolist (node-pos '((5 5) (5 6) (5 7)
		      (6 5) (6 6) (6 7)
		      (7 5) (7 6) (7 7)))
    (my-cellular-automata-set-cell (car node-pos) (cadr node-pos) *my-cellular-automata-node-char*)
    ))

(defvar my-cellular-automata-mode-map nil "local keymap for cellular automata mode")
(setq my-cellular-automata-mode-map (make-sparse-keymap))

(define-key my-cellular-automata-mode-map (kbd "SPC") 'my-cellular-automata-place-node)

(define-key my-cellular-automata-mode-map (kbd "u") 'my-cellular-automata-update)
(define-key my-cellular-automata-mode-map (kbd "s") 'my-cellular-automata-start-auto-update)
(define-key my-cellular-automata-mode-map (kbd "c") 'my-cellular-automata-cancel-auto-update)

(define-derived-mode my-cellular-automata-mode special-mode
  "my cellular automata mode"
  "a mode for playing cellular automata"
  )

(add-hook 'quit-window-hook 'my-cellular-automata-cancel-auto-update)
(defvar *my-cellular-automata-field* nil
  "cellular automata field")

(defvar *my-cellular-automata-field-size* nil
  "cellular automata field size")

(defvar *my-cellular-automata-node-char* ?\@
  "a character which represents a node")

(defvar *my-cellular-automata-blank-char* ?\.
  "a character which represents a blank cell")

(defun my-cellular-automata-init ()
  "init the cellular automata field"
  (setq
   *my-cellular-automata-field-size* 20
   *my-cellular-automata-field* (make-vector (* *my-cellular-automata-field-size* *my-cellular-automata-field-size*) *my-cellular-automata-blank-char*))
  (if *my-cellular-automata-timer*
      (my-cellular-automata-cancel-auto-update))
  )

(defun my-cellular-automata-get-cell (row column)
  "get cell value at row, column"
  (elt *my-cellular-automata-field*
       (+ column (* row *my-cellular-automata-field-size*)))
  )

(defun my-cellular-automata-set-cell (row column value)
  "set cell to value at row, column"
  (aset *my-cellular-automata-field*
	(+ column (* row *my-cellular-automata-field-size*))
	value
	)
  )

(defun my-cellular-automata-place-node ()
  "put a node at current point"
  (interactive)
  (let ((cwidth (string-width (format "%c" *my-cellular-automata-node-char*))))
    (my-cellular-automata-set-cell (1- (line-number-at-pos)) (/ (current-column) cwidth) *my-cellular-automata-node-char*)
    )
  (let ((p (point)))
    (my-cellular-automata-show-field)
    (goto-char p)
  ))

(defun my-cellular-automata-show-field ()
  "show the current CA field"
  (with-current-buffer *my-cellular-automata-buffer-name*
    (let ((inhibit-read-only t) (p (point)))
      (erase-buffer)
      (dotimes (row *my-cellular-automata-field-size*)
	(dotimes (column *my-cellular-automata-field-size*)
	  (insert (my-cellular-automata-get-cell row column)))
	(insert "\n"))
      (goto-char p)
      (highlight-regexp (format "%c" *my-cellular-automata-node-char*) 'hi-blue)
      (font-lock-fontify-buffer)
      )))

(defun my-cellular-automata-value-in-range (value)
  "return a value in range based on the input value"
  (let ((max-value (1- *my-cellular-automata-field-size*)))
  (cond ((> value max-value)
	(% value max-value))
	((< value 0) (+ max-value (% value max-value)))
	 (t value))))
  
(defun my-cellular-automata-is-cell-taken (row column)
  "check if a cell is occupied.  row and column are adjusted to fit in range."
  (eq (my-cellular-automata-get-cell (my-cellular-automata-value-in-range row) (my-cellular-automata-value-in-range column)) *my-cellular-automata-node-char*)
  )

(defun my-cellular-automata-is-alive-in-next-turn (row column)
  "check if the node should be alive in next turn"
  (and t
       (let ((c 0))
	 (dotimes (i 3)
	   (dotimes (j 3)
	     (setq c (+ c
			(if (my-cellular-automata-is-cell-taken (+ (1- row) i) (+ (1- column) j))
			    1
			  0)))
	     )) (and (< c 5) (> c 1)))))

(defun my-cellular-automata-update (&optional num-repetitions)
  "update cells"
  (interactive "p")
  (dotimes (i num-repetitions)
    (let ((tmp-field (copy-sequence *my-cellular-automata-field*)))
      (dotimes (row *my-cellular-automata-field-size*)
	(dotimes (column *my-cellular-automata-field-size*)
	  (aset tmp-field
		(+ column (* row *my-cellular-automata-field-size*))
		(if (my-cellular-automata-is-alive-in-next-turn row column)
		    *my-cellular-automata-node-char*
		  *my-cellular-automata-blank-char*
		  ))))
      (setq *my-cellular-automata-field* (copy-sequence tmp-field))
      )
    (my-cellular-automata-show-field)))

(defun my-cellular-automata-start-auto-update ()
  "start updating automatically"
  (interactive)
  (if (not *my-cellular-automata-timer*)
      (setq *my-cellular-automata-timer* (run-with-timer 1 1 'my-cellular-automata-update 1))) nil)

(defun my-cellular-automata-cancel-auto-update ()
  "cancel updating automatically"
  (interactive)
  (if *my-cellular-automata-timer*
      (progn
	(cancel-timer *my-cellular-automata-timer*)
	(setq *my-cellular-automata-timer* nil)
	)))

(provide 'my-cellular-automata)
;;; my-cellular-automata.el ends here
