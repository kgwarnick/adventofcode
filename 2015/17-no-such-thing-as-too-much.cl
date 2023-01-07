;;; Advent of Code 2015 Day 17: No Such Thing as Too Much
;;; https://adventofcode.com/2015/day/17

;; Try the next container and return the number of solutions found
(defun try-containers (amount containers used)
  ; Try all available containers
  (do* ((i 0 (+ i 1))
        (numsol 0 numsol))
    ((>= i (list-length containers)) numsol)
    ; (princ "Containers used ") (princ used) (princ ",  ")
    ; (princ "Containers available: ") (princ containers) (terpri)
    (let ((c (elt containers i)))
      ; (princ "- trying container ") (princ c) (terpri)
      (if (= amount c)   ; Found another solution
        (progn
          ; (princ "- Solution: ") (princ (cons c used)) (terpri)
          (setf numsol (+ numsol 1)) ))
      (if (> amount c)   ; Use this container and keep searching
        ; in the rest of the container list
        (setf numsol (+ numsol
                        (try-containers
                          (- amount c)
                          (subseq containers (+ i 1))
                          (cons c used)) )))
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


(princ "-- Tests ---") (terpri)
(princ "Amount 5 in containers (10 5 2)")  (terpri)
(defvar ergebnis (try-containers 5 '(10 5 2) '()))
(princ "Combinations: ")  (princ ergebnis)  (terpri)
(princ "Amount 10 in containers (10 5 2 5)")  (terpri)
(setf ergebnis (try-containers 10 '(10 5 2 5) '()))
(princ "Combinations: ")  (princ ergebnis)  (terpri)
(terpri)

(princ "--- Example ---") (terpri)
(defvar example-amount 25)
(defvar example-containers '(20 15 10 5 5))
(defvar example-number-of-solutions 4)
(princ "Containers: ")  (princ example-containers)  (terpri)
(setf ergebnis (count-solutions example-amount example-containers))
(princ "Combinations: ")  (princ ergebnis)  (terpri)
(princ "Result should be: ")  (princ example-number-of-solutions)  (terpri)
(terpri)

(princ "--- Puzzle 1 ---") (terpri)
(defvar input-amount 150)
(defvar input-containers
  (mapcar 'parse-integer
          (get-file-content "17-no-such-thing-as-too-much-input.txt")))
(princ "Amount 5 in ")  (princ (list-length input-containers))
(princ " containers ")  (princ input-containers)  (terpri)
(setf ergebnis (count-solutions input-amount input-containers))
(princ "Combinations: ")  (princ ergebnis)
(terpri)
