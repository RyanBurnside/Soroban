;;; Ryan Burnside 2014
;;; Emacs Soroban (Japanese Abacus)
;;; Permission is granted to hack this freely released under GPL3
;;; m-x soroban will start Soroban Mode
;;; Spacebar to push a bead when the cursor is over it
;;; c will clear the soroban
;;; Mouse button 1 moves the bead underneath
;;; Mouse button 3 clears the soroban

(require 'cl)

(defun soroban ()
  "Start the Emacs soroban"
  (interactive)
  (switch-to-buffer "Emacs Soroban")
  (soroban-mode)
  (soroban-init)
  (soroban-print-buffer))

(defstruct bead-spindle (x 0) (y 0) (beads (vector 1 0 0 1 1 1 1)))
(defvar *soroban-buffer* nil "The screen buffer for the soroban")
(defvar *soroban-rods* nil)

(defun soroban-init ()
  "Create the buffer to hold the screen images"
  (setq *num-soroban-columns* 19)
  (setq *buffer-height* 10)
  (setq *buffer-width* 79)
  (setq *soroban-buffer* (make-vector (* *buffer-width* *buffer-height*) ?\ ))
  (setq *soroban-rods* (loop for i from 0 to (1- *num-soroban-columns*)
			    collecting (make-bead-spindle :x (* i 4) :y 0
						:beads (vector 1 0 0 1 1 1 1))))
  (soroban-print-buffer))

(define-derived-mode soroban-mode special-mode "soroban-mode"
  (define-key soroban-mode-map (kbd "SPC") 'move-bead)
  (define-key soroban-mode-map (kbd "c") 'soroban-clear)
  (define-key soroban-mode-map (kbd "<mouse-1>") 'move-bead)
  (define-key soroban-mode-map (kbd "<mouse-3>") 'soroban-clear))

(defun soroban-get-value (b)
  (let ((v 0))
    (if (= (aref (bead-spindle-beads b) 1) 1)
	(incf v 5))
    (loop for i from 2 to 6 do
	  (if (= (aref (bead-spindle-beads b) i) 1)
	      (incf v)
	      (return)))
    v))

(defun soroban-set-clicked (rod-index event-y)
  "Uses y position of click event to determine bead pattern"
  (let* ((temp (nth rod-index *soroban-rods*))
	 (delta (- event-y (bead-spindle-y temp))))
    (cond ((= delta 0)
	   (setf (aref (bead-spindle-beads temp) 0) 0)
	   (setf (aref (bead-spindle-beads temp) 1) 1))
	  ((= delta 1)
	   (setf (aref (bead-spindle-beads temp) 0) 1)
	   (setf (aref (bead-spindle-beads temp) 1) 0))
	  ((> delta 2)
	   (let ((a (aref (bead-spindle-beads temp) 0))
		 (b (aref (bead-spindle-beads temp) 1)))
	     (fillarray (bead-spindle-beads temp) 1)
	     (setf (aref (bead-spindle-beads temp) 0) a)
	     (setf (aref (bead-spindle-beads temp) 1) b)
	     (setf (aref (bead-spindle-beads temp) (- delta 1)) 0))))))

(defun soroban-spindle-clicked (x y)
  "Updates a clicked spindle"
  (loop for i from 0 to (- (length *soroban-rods*) 1) do
	(let ((temp (nth i *soroban-rods*)))
	  (and (>= x (bead-spindle-x temp))
	       (>= y (bead-spindle-y temp))
	       (<= x (+ (bead-spindle-x temp) 2))
	       (<= y (+ (bead-spindle-y temp) 7))
	       (soroban-set-clicked i y)))))

(defun soroban-buffer-get-char(x y)
  (elt *soroban-buffer*
       (+ x
	  (* y *buffer-width*))))

(defun soroban-buffer-set-char(x y value)
  (aset *soroban-buffer*      
	(+ x
	  (* y *buffer-width*))
       value))

(defun soroban-draw-dots ()
  "Marks decimal dots on soroban bar"
  (loop with n = 0 
	for i in (reverse *soroban-rods*) do
	(incf n)
	(if (= (mod n 3) 0)
	    (soroban-buffer-set-char (1+ (bead-spindle-x i))
				 (+ (bead-spindle-y i) 2)
				 ?\*))))

(defun soroban-draw-bead (x y)
  "Draw a 3 character bead <_>"
  (soroban-buffer-set-char x y ?\<)
  (soroban-buffer-set-char (1+ x) y ?\_)
  (soroban-buffer-set-char (+ x 2) y ?\>))

(defun soroban-draw-rod (x y)
  "Draw a 3 character bead  | "
  (soroban-buffer-set-char (1+ x) y ?\|))

(defun soroban-draw-underscore (y)
  "Used to make the bar across the screen"
  (dotimes (i (1- (* *num-soroban-columns* 4)))
    (soroban-buffer-set-char i y ?\_)))

(defun soroban-draw-spindle (spindle)
  "Draws a spindle structure"
  (let ((x (bead-spindle-x spindle))
	(y (bead-spindle-y spindle)))
    (dotimes (i 2) ; Top two bead slots
      (if (= (aref (bead-spindle-beads spindle) i) 0)
	  (soroban-draw-rod x (+ y i))
          (soroban-draw-bead x (+ y i))))
    (dotimes (i 5)
      (if (= (aref (bead-spindle-beads spindle) (+ i 2)) 0)
	  (soroban-draw-rod x (+ y i 3))
          (soroban-draw-bead x (+ y i 3))))
    (soroban-buffer-set-char (1+ x) (+ y 8) (string-to-char
					     (number-to-string
					     (soroban-get-value spindle))))))

(defun soroban-print-buffer()
  "Clear screen, blit the buffer onto the screen"
  (let ((inhibit-read-only t)
	(end-pos (point)))
    (fillarray *soroban-buffer* ?\ )
    (erase-buffer)
    (soroban-draw-underscore 1)
    (soroban-draw-underscore 2)
    (soroban-draw-underscore 7)
    (soroban-draw-dots)
    (dotimes (i (length *soroban-rods*))
      (soroban-draw-spindle (nth i *soroban-rods*)))
    (dotimes (row *buffer-height*)
      (dotimes (column *buffer-width*)
	(insert (soroban-buffer-get-char column row)))
      (insert "\n"))
    ;; Need to go to the former cursor position now
    (goto-char end-pos)))

(defun soroban-clear ()
  (interactive)
  (loop for n in *soroban-rods* do 
	  (setf (bead-spindle-beads n) (vector 1 0 0 1 1 1 1)))
  (soroban-print-buffer))

(defun move-bead()
  "Finds the bead of the spindle clicked, sets to 0"
  (interactive)
  (let ((row (1- (line-number-at-pos)))
	(column (current-column)))
    (soroban-spindle-clicked column row))
  (soroban-print-buffer))
    
