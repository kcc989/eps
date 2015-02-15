;;; Zach Arnold

;;; Elevator Simulation

(requires "gps")

;;; ==== CODE FOR ELEVATOR SIMULATION ====

(defun my-reverse (x) ;reverses the list x
       (if (null x)
       nil
       (append (my-reverse (cdr x)) (cons (car x) nil))))

(defun range (x y) ;; generate (x x+1 ... y)
       (cond
       ((= x y) (cons x nil))
       ((< x y)(cons x (range (+ x 1) y)))
       (t (cons x (range (- x 1) y)))
       )
)

;; constructs all operations from floor to floor
;(defun make-elevator-ops (top) 
;  (loop for i in (reverse (range 1 top)) do 
;    (push (make-elevator-op i (+ i 1)) *elevatorOps*)
;    (push (make-elevator-op (+ i 1) i) *elevatorOps*))
;)

(defun make-elevator-ops (top) 
  (loop for i in (reverse (range 1 (+ top 1))) do
	(loop for j in (reverse (range 1 (+ 1 top))) do
	      (if (not (= i j)) 
		(push (make-elevator-op i j) *elevatorOps*)))))

;; helper for the above function
(defun make-elevator-op (a b) 
  "Make an operator to move from floor a to b."
  (op `(,a to ,b) ;; moving from floor a to floor b
      :preconds `((on ,a)(door-closed))
      :add-list `((on ,b))
      :del-list `((on ,a))))

;; make all combinations of on floor to floor ops
(defun make-on-ops (top) 
  (loop for i in (reverse (range 1 (+ top 1))) do
	(loop for j in (reverse (range 1 (+ 1 top))) do
	      (if (not (= i j)) 
		(make-on-ops-h i j)))))

(defun make-on-ops-h (floor-on floor-want)
  (push (make-get-on-op floor-on floor-want) *elevatorOps*))

;; make all combinations of off ops 
(defun make-off-ops (top) 
  (loop for i in (reverse (range 1 (+ top 1))) do
	(make-off-ops-h i)))

(defun make-off-ops-h (floor-want)
  (push (make-get-off-op floor-want) *elevatorOps*))

;; getting person on at the current floor
(defun make-get-on-op (floor-on floor-want)
  (op `(load-on ,floor-on) 
      :preconds `((door-opened)(on ,floor-on)(person-on ,floor-on wants ,floor-want))
      :add-list `((person-aboard-wants ,floor-want))
      :del-list `((person-on ,floor-on wants ,floor-want))))

;; off loading person at current floor
(defun make-get-off-op (floor-want)
  (op `(drop-off-on ,floor-want) 
      :preconds `((door-opened)(on ,floor-want)(person-aboard-wants ,floor-want))
      :add-list `((person-delivered-to ,floor-want))
      :del-list `((person-aboard-wants ,floor-want))))


(defparameter *elevatorOps* '()) ;; list of elevator ops
(defparameter *floors* 20) ;; desired number of floors

(make-elevator-ops *floors*) ;; b/w floors ops

(make-on-ops *floors*) ;;pick up and ...
(make-off-ops *floors*) ;;drop off ops

;;open and close ops
(push (op '(open-door)
               :preconds '((door-closed))
               :add-list '((door-opened))
               :del-list '((door-closed)))
      *elevatorOps*)

(push (op '(close-door)
               :preconds '((door-opened))
               :add-list '((door-closed))
	       :del-list '((door-opened)))
      *elevatorOps*)

;; TESTING HARNESS -------------------------

(defun spaces ()
       (cond
       ( t (terpri))
       ( t (terpri))

 ))

(defun display (stuff)
       (princ stuff)(terpri)
)

(defun test-fun (fun parms)

       (spaces)
       (display "Function call : ")
       (display (cons fun parms))
       (spaces)
       (display "Result:")
 )

(spaces)
(spaces)

(display "Demonstrating examples of elevator simulation ...")
(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
(spaces)
(display "Elevator traveling from floor to floor:")
(setq parms `(((on 6)(door-closed)) ((on 1)(door-opened))))
(test-fun 'gps parms)
(print (gps  '((on 6)(door-closed)) '((on 1)(door-opened))  *elevatorOps*))
(spaces)
(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
(spaces)
(display "Elevator traveling from floor to floor and picking up a passenger:")
(setq parms `(((person-on 3 wants 1)(door-closed)(on 6)) ((person-aboard-wants 1) (door-closed))))
(test-fun 'gps parms)
(print (gps '((person-on 3 wants 1)(door-closed)(on 6)) '((person-aboard-wants 1) (door-closed))  *elevatorOps*))
(spaces)
(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
(spaces)
(display "Elevator traveling from floor to floor, picking up a passenger and dropping them off:")
(setq parms `(((person-on 1 wants 5) (door-closed) (on 2)) ((person-delivered-to 5))))
(test-fun 'gps parms)
(print (gps  '((person-on 1 wants 5) (door-closed) (on 2)) '((person-delivered-to 5)) *elevatorOps*))
(spaces)
(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
(spaces)
(display "Multiple passenger requests:")
(setq parms `(((person-on 2 wants 17) (door-closed) (on 5) (person-on 16 wants 1)) 
((person-delivered-to 17) (person-delivered-to 1))))
(test-fun 'gps parms)
(print (gps  '((person-on 2 wants 17) (door-closed) (on 5) (person-on 16 wants 1)) 
'((person-delivered-to 17) (person-delivered-to 1)) *elevatorOps*))
(spaces)
(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")







