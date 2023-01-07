;;; Advent of Code 2015 Day 17: No Such Thing as Too Much
;;; https://adventofcode.com/2015/day/17

;; Try the next container and return a list of all solutions
(defun try-containers (amount containers used)
  ; Try all available containers
  (do ((i 0 (+ i 1))
       (solutions '() solutions))
    ((>= i (list-length containers)) solutions)
    ; (princ "Containers used ") (princ used) (princ ",  ")
    ; (princ "Containers available: ") (princ containers) (terpri)
    (let ((c (elt containers i)))
      ; (princ "- trying container ") (princ c) (terpri)
      (if (= amount c)   ; Found another solution
        (progn
          ; (princ "- Solution: ") (princ (cons c used)) (terpri)
          (setf solutions (cons (append used (list c)) solutions)) ))
      (if (> amount c)   ; Use this container and keep searching
        (setf solutions (append solutions
                        (try-containers
                          (- amount c)
                          (subseq containers (+ i 1))
                          (append used (list c))) )))
      ; (if (< amount c)
      ;   (progn
      ;     (princ "Container ") (princ c) (princ " too large for rest ")
      ;     (princ amount) (terpri) ))
      )
    )
  )


;; Count the number of solutions to divide the amount into available containers
(defun count-solutions (amount containers)
  (try-containers amount containers '()))


;; Read input file and return its contents
(defun get-file-content (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect line)))


;; Find number of combinations with minimum number of containers
(defun count-min-combinations (solutions)
  (let ((minimumnum (apply 'min (mapcar 'list-length solutions))))
    (princ "- minimum number of containers: ")  (princ minimumnum)
    (terpri)
    (count-if (lambda (el) (= (list-length el) minimumnum)) solutions))
  )


(princ "-- Tests ---") (terpri)
(princ "Amount 5 in containers (10 5 2)")  (terpri)
(defvar ergebnis (try-containers 5 '(10 5 2) '()))
(princ "Combinations: ")  (princ (list-length ergebnis))  (princ ", ")
(princ ergebnis)  (terpri)
(defvar withmincont (count-min-combinations ergebnis))
(princ "Combinations with minimum number of containers: ")
(princ withmincont) (terpri)
(princ "Amount 10 in containers (10 5 2 5)")  (terpri)
(setf ergebnis (try-containers 10 '(10 5 2 5) '()))
(princ "Combinations: ")  (princ (list-length ergebnis))  (princ ", ")
(princ ergebnis)  (terpri)
(setf withmincont (count-min-combinations ergebnis))
(princ "Combinations with minimum number of containers: ")
(princ withmincont) (terpri)
(terpri)

(princ "--- Example ---") (terpri)
(defvar example-amount 25)
(defvar example-containers '(20 15 10 5 5))
(defvar example-number-of-solutions 4)
(princ "Containers: ")  (princ example-containers)  (terpri)
(setf ergebnis (count-solutions example-amount example-containers))
(princ "Combinations: ")  (princ (list-length ergebnis))  (princ ", ")
(princ ergebnis)  (terpri)
(princ "Result should be: ")  (princ example-number-of-solutions)  (terpri)
(terpri)

(setf withmincont (count-min-combinations ergebnis))
(princ "Combinations with minimum number of containers: ")
(princ withmincont) (terpri)
(terpri)

(princ "--- Puzzle 1: Number of combinations ---") (terpri)
(defvar input-amount 150)
(defvar input-containers
  (mapcar 'parse-integer
          (get-file-content "17-no-such-thing-as-too-much-input.txt")))
(princ "Amount ")  (princ input-amount)  (princ " in ")
(princ (list-length input-containers))
(princ " containers ")  (princ input-containers)  (terpri)
(setf ergebnis (count-solutions input-amount input-containers))
(princ "Combinations: ")  (princ (list-length ergebnis))  (terpri)
(terpri)

(princ "--- Puzzle 2: Combinations with minimum number of containers ---")
(terpri)
(setf withmincont (count-min-combinations ergebnis))
(princ "Combinations with minimum number of containers: ")
(princ withmincont) (terpri)
