;;; Zach Arnold and Casey Collins

;;; Elevator Simulation

(requires "gps")

;;; === GLOBALS ===

(defparameter *elevatorOps* '()) ;; list of elevator ops
(defparameter *floors* 19) ;; desired number of floors
(defparameter *optimizedGoals* '()) ;; for pre-processing
(defparameter *upp* t)
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

(defun make-on-ops-h (floor-on floor-want)
  (push (make-get-on-op floor-on floor-want) *elevatorOps*))

;; make all combinations of off ops 
(defun make-off-ops (top) 
  (loop for i in (reverse (range 1 (+ top 1))) do
	(make-off-ops-h i)))

(defun make-off-ops-h (floor-want)
  (push (make-get-off-op floor-want) *elevatorOps*))

;; pickup `(person-aboard-wants ,floor)
;; drop off `(person-delivered-to ,floor)

;;; Location Sensor 

(defun get-direction (pair current)
  "Return the direction of the request being considered will move the elevator"
  (if (> 0 (- current (car pair)))
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
   ((null pick) (merge-ups pick (rest drop) (push `(person-delivered-to ,(cadar drop)) goals)))
   ((null drop) (merge-ups (rest pick) drop (push `(loaded-person-for ,(cadar pick)) goals)))
   ((< (first (first pick)) (second (first drop))) (merge-ups (rest pick) drop (push `(loaded-person-for ,(cadar pick)) goals)))
   (t (merge-ups pick (rest drop) (push `(person-delivered-to ,(cadar drop)) goals)))))
	
(defun merge-downs (pick drop goals)
  "Merge the elevator plan lists for downward requests"
  (cond
   ((and (null pick) (null drop)) (reverse goals))
   ((null pick) (merge-downs pick (rest drop) (push `(person-delivered-to ,(cadar drop)) goals)))
   ((null drop) (merge-downs (rest pick) drop (push `(loaded-person-for ,(cadar pick)) goals)))
   ((> (first (first pick)) (second (first drop))) (merge-downs (rest pick) drop (push `(loaded-person-for ,(cadar pick)) goals)))
   (t (merge-downs pick (rest drop) (push `(person-delivered-to ,(cadar drop)) goals)))))


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
      :add-list `((person-aboard-wants ,floor-want)(loaded-person-for ,floor-want))
      :del-list `((person-on ,floor-on wants ,floor-want))))

;; off loading person at current floor
(defun make-get-off-op (floor-want)
  (op `(drop-off-on ,floor-want) 
      :preconds `((door-opened)(on ,floor-want)(person-aboard-wants ,floor-want))
      :add-list `((person-delivered-to ,floor-want))
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

(defun get-pickups (lst)
  (if (equal lst nil) '()
    (cons (caar lst) (get-pickups (rest lst))))
)

(defun get-drop-offs (lst)
  (if (equal lst nil) '()
    (cons (second (first lst)) (get-drop-offs (rest lst))))
)

(defun sort-by-car-up (lst) ;; sort for going up
  (display 1)
  
  (sort lst (lambda (a b) (< (car a) (car b))))
)

(defun sort-by-car-down (lst) ;;sort for going down
  (display 2)
  
  (sort lst (lambda (a b) (> (car a) (car b))))
)

(defun sort-by-second-up (lst) ;; sort for going up
  (display 3)
  

  (sort lst (lambda (a b) (< (second a) (second b))))
)

(defun sort-by-second-down (lst) ;;sort for going down
  (display 4)
  
  (sort lst (lambda (a b) (> (second a) (second b))))
)

(defun get-ups (lst)
  (if (equal lst nil) '()
  (let ((tmp (first lst)))
    (if (< (first tmp) (second tmp))
	(cons tmp (get-ups (rest lst)))
      (get-ups (rest lst))))))

(defun get-downs (lst)
  (if (equal lst nil) '()
  (let ((tmp (first lst)))
    (if (> (first tmp) (second tmp))
	(cons tmp (get-downs (rest lst)))
      (get-downs (rest lst))))))

(defun construct-path (dir-lst) ;; given a list od goals in the same direction construct the goals
  (sort-by-car)
)
      
(defun get-floors (lst)
  (if (equal (car lst) 'on) (setf *startFloor* (second lst)))
  (if (equal (car lst) 'person-on)
      (cons (second lst) (cons (fourth lst) nil))
    nil))

(defun generate-goals (lstup-on lstup-off lstdown-on lstdown-off)
  (display lstup-on)
  (display lstup-off)
  (display lstdown-on)
  (display lstdown-off)
  (merge-lists lstup-on lstup-off lstdown-on lstdown-off)
)

(defun fix-goals (lst) ;re-write goals base on pre conditions
  (setq result '())
  (let ((temp (car lst)))
    (loop for thing in temp do 
	  (let ((part (get-floors thing)))
	    (cond 
	     ((not (equal part nil))(push part result))))))

(list (car lst)
  (generate-goals (sort-by-car-up (get-ups result)) (sort-by-second-up (get-ups result)) (sort-by-car-down 
(get-downs result)) (sort-by-second-down (get-downs result)))))

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

;(display "Demonstrating examples of elevator simulation ...")
;(spaces)
;(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
;(spaces)
;(display "Multiple passenger requests:")
;;;;(setq parms `(((person-on 2 wants 17) (door-closed) (on 5) (person-on 16 wants 1))
;;;((person-delivered-to 17) (person-delivered-to 1))))
;(test-fun 'gps parms)
;(print (gps  '( (door-closed) (on 5) (person-on 16 wants 1) (person-on 2 wants 17)) 
;'( (person-delivered-to 1)(person-delivered-to 17)) *elevatorOps*))
;(spaces)
;(spaces)
;(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
;(fix-goals parms)

;(display (get-ups '((1 5) (3 4) (5 7) (8 4))))

(setq parms `((person-on 2 wants 17) (person-on 4 wants 5) (person-on 3 wants 19) (person-on 7 wants 6) (door-closed) (on 1) (person-on 16 wants 1)))

;(debug2 :gps)


;(print (gps '((person-on 3 wants 1) (person-on 5 wants 2)(door-closed)(on 6)) '((person-aboard-wants 2)(person-aboard-wants 1)(person-delivered-to 2)(door-closed))  *elevatorOps*))
(let ((x (fix-goals (cons parms '((blah blah))))))
(display (gps (first x) (second x) *elevatorOps*)))




;(display (sort-by-car-up '((1 2) (3 4) (2 3))))
;(display (sort-by-car-down '((1 2) (3 4) (2 3))))
;(display (sort-by-second-up '((1 2) (3 4) (2 3))))
;(display (sort-by-second-down '((1 2) (3 4) (2 3))))






