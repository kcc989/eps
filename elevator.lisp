;;; Zach Arnold and Casey Collins

;;; Elevator Simulation

(requires "gps")

;;; === GLOBALS ===

(defparameter *elevatorOps* '()) ;; list of elevator ops
(defparameter *floors* 19) ;; desired number of floors
(defparameter *optimizedGoals* '()) ;; for pre-processing
;(defparameter *upp* t)
(defparameter *startFloor* 0)


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

(defun make-off-ops (top) 
  (loop for i in (reverse (range 1 (+ top 1))) do
	(loop for j in (reverse (range 1 (+ 1 top))) do
	      (if (not (= i j)) 
		(make-off-ops-h i j)))))


(defun make-on-ops-h (floor-on floor-want)
  (push (make-get-on-op floor-on floor-want) *elevatorOps*))


(defun make-off-ops-h (floor-on floor-want)
  (push (make-get-off-op floor-on floor-want) *elevatorOps*))

;; make all combinations of off ops 
;(defun make-off-ops (top) 
;  (loop for i in (reverse (range 1 (+ top 1))) do
;	(make-off-ops-h i)))

;(defun make-off-ops-h (floor-want)
;  (push (make-get-off-op floor-want) *elevatorOps*))

;; pickup `(person-aboard-wants ,floor)
;; drop off `(person-delivered-to ,floor)

;;; Location Sensor 

(defun get-direction (pair current)
  "Return the direction of the request being considered will move the elevator"
  (if (< (- (first pair) (second pair)) 0)
      'up
    'down))

(defun direction-helper (closest direction floorlist current)
  "Find the direction the elevator should currently be moving"
  (cond 
   ((null floorlist) direction)
   ((< (abs (- current (caar floorlist))) (- current closest)) (direction-helper (caar floorlist) (get-direction (car floorlist) current) (cdr floorlist) current))
   (t (direction-helper closest direction (cdr floorlist) current))))
      

(defun choose-direction (floorlist current)
  "Calls the function that will determine the direction the elevator starts moving in"
  (setq closest (caar floorlist))
  (direction-helper closest (get-direction (car floorlist) current) (cdr floorlist) current))
  

;;;
;;; CASEY's MERGE LISTS AREA!!!!!!!
;;;
(defun merge-ups (pick drop goals)
  "Merge the the elevator plan lists for the upward requests"
  (cond
   ((and (null pick) (null drop)) (reverse goals))
   ((null pick) (merge-ups pick (rest drop) (push `(person-delivered-to ,(cadar drop) from ,(caar drop)) goals)))
   ((null drop) (merge-ups (rest pick) drop (push `(loaded-person-for ,(cadar pick) from ,(caar pick)) goals)))
   ((< (first (first pick)) (second (first drop))) (merge-ups (rest pick) drop (push `(loaded-person-for ,(cadar pick) from ,(caar pick)) goals)))
   (t (merge-ups pick (rest drop) (push `(person-delivered-to ,(cadar drop) from ,(caar drop)) goals)))))
	
(defun merge-downs (pick drop goals)
  "Merge the elevator plan lists for downward requests"
  (cond
   ((and (null pick) (null drop)) (reverse goals))
   ((null pick) (merge-downs pick (rest drop) (push `(person-delivered-to ,(cadar drop) from ,(caar drop)) goals)))
   ((null drop) (merge-downs (rest pick) drop (push `(loaded-person-for ,(cadar pick) from ,(caar pick)) goals)))
   ((> (first (first pick)) (second (first drop))) (merge-downs (rest pick) drop (push `(loaded-person-for ,(cadar pick) from ,(caar pick)) goals)))
   (t (merge-downs pick (rest drop) (push `(person-delivered-to ,(cadar drop) from ,(caar drop)) goals)))))


(defun merge-lists (up-on up-off down-on down-off)
  "Merge the elevator plans we preprocessed into one coherrent list of goals"
  (if (equal 'up (choose-direction (append up-on down-on) *startFloor*))
     (append (merge-ups up-on up-off '()) (merge-downs down-on down-off '()))
 (append (merge-downs down-on down-off '()) (merge-ups up-on up-off '()))))
;;; end of merge list area
;;;

;; getting person on at the current floor
(defun make-get-on-op (floor-on floor-want)
  (op `(load-on ,floor-on) 
      :preconds `((door-opened)(on ,floor-on)(person-on ,floor-on wants ,floor-want))
      :add-list `((person-aboard-wants ,floor-want)(loaded-person-for ,floor-want from ,floor-on))
      :del-list `((person-on ,floor-on wants ,floor-want))))

;; off loading person at current floor
(defun make-get-off-op (floor-initial floor-want)
  (op `(drop-off-on ,floor-want) 
      :preconds `((door-opened)(on ,floor-want)(person-aboard-wants ,floor-want))
      :add-list `((person-delivered-to ,floor-want from ,floor-initial))
      :del-list `((person-aboard-wants ,floor-want))))

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





;; toggle direction ops

;---------------PRE-PROCESSING FUNCTIONS -----------------------

;;; get all the pickup locations
(defun get-pickups (lst)
  (if (equal lst nil) '()
    (cons (caar lst) (get-pickups (rest lst))))
)

;;; get all the drop off locations
(defun get-drop-offs (lst)
  (if (equal lst nil) '()
    (cons (second (first lst)) (get-drop-offs (rest lst))))
)

;;; sort by the first item in list for the down list
(defun sort-by-car-up (lst) 
  (sort lst (lambda (a b) (< (car a) (car b))))
)
;;; sort by the first item in list for the down list
(defun sort-by-car-down (lst) 
  (sort lst (lambda (a b) (> (car a) (car b))))
)

;;; sort by second item in list for the up list
(defun sort-by-second-up (lst) 
  (sort lst (lambda (a b) (< (second a) (second b))))
)

;;; sort by second item in list for the down list
(defun sort-by-second-down (lst) 
  (sort lst (lambda (a b) (> (second a) (second b))))
)

;;; given an assosciation list this returns only the assosciations that 
;;; are going up
(defun get-ups (lst)
  (if (equal lst nil) '()
  (let ((tmp (first lst)))
    (if (< (first tmp) (second tmp))
	(cons tmp (get-ups (rest lst)))
      (get-ups (rest lst))))))

;;; given an assosciation list this returns only the assosciations that 
;;; are going down
(defun get-downs (lst)
  (if (equal lst nil) '()
  (let ((tmp (first lst)))
    (if (> (first tmp) (second tmp))
	(cons tmp (get-downs (rest lst)))
      (get-downs (rest lst))))))

;; given a list of goals in the same direction construct the goals for eps
(defun construct-path (dir-lst) 
  (sort-by-car)
)

;;; This function collects all the floor requests and
;;; where they would like to go and puts them into an 
;;; assosciation list
(defun get-floors (lst)
  (if (equal (car lst) 'on) (setf *startFloor* (second lst)))
  (if (equal (car lst) 'person-on)
      (cons (second lst) (cons (fourth lst) nil))
    nil))


;;; This is just a helper function that calls the function where the 
;;; actual merging happens

(defun generate-goals (lstup-on lstup-off lstdown-on lstdown-off)
  (merge-lists lstup-on lstup-off lstdown-on lstdown-off)
)

;;; This function will determine the best goals to use in conjunction
;;; with gps. It will also determine the best direction
(defun fix-goals (lst) 
  (setq result '())
  (let ((temp (car lst)))
    (loop for thing in temp do 
	  (let ((part (get-floors thing)))
	    (cond 
	     ((not (equal part nil))(push part result))))))

  (list (car lst)
  (generate-goals (sort-by-car-up (get-ups result)) (sort-by-second-up (get-ups result)) (sort-by-car-down 
  (get-downs result)) (sort-by-second-down (get-downs result)))))

(defun eps (parms) 
  (let ((x (fix-goals (cons parms '((blah blah))))))
    (gps (first x) (second x) *elevatorOps*)))


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

(setq parms `((person-on 2 wants 17) (person-on 4 wants 5) (person-on 3 wants 19) (person-on 7 wants 6) (door-closed) (on 1) (person-on 16 wants 1)))

(display "Demonstrating examples of elevator simulation ...")
(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
(spaces)
(display "Demonstrating correct traversal")
(test-fun 'eps parms)
(display (eps parms))
(spaces)
(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

(setq parms `((person-on 16 wants 1)(person-on 2 wants 17) (person-on 4 wants 5) (person-on 3 wants 19) (person-on 17 wants 6) (door-closed) (on 15)))

(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
(spaces)
(display "Demonstration of logic to start in the right direction:")
(test-fun 'eps parms)
(display (eps parms))
(spaces)
(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

(setq parms `((door-closed)(person-on 2 wants 5) (person-on 1 wants 5)(person-on 6 wants 7)(on 15)))

(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
(spaces)
(display "Demonstration of same floor drop off:")
(test-fun 'eps parms)
(display (eps parms))
(spaces)
(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

(setq parms `((door-closed)(person-on 2 wants 5) (person-on 2 wants 4)(person-on 6 wants 7)(on 15)))

(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
(spaces)
(display "Demonstration of same floor pickup:")
(test-fun 'eps parms)
(display (eps parms))
(spaces)
(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")


(setq parms `((door-closed)(person-on 2 wants 3)(person-on 6 wants 3)(on 1)))

(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
(spaces)
(display "Demonstration of different direction drop off on same floor:")
(test-fun 'eps parms)
(display (eps parms))
;(display (fix-goals (cons parms '((blah blah)))))
(spaces)
(spaces)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")


