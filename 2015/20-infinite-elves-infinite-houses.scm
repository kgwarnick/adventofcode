;; Advent of Code 2015 Day 20: Infinite Elves and Infinite Houses
;; https://adventofcode.com/2015/day/20


;; Is n divisible by m
;; 
(define is-divisible
  (lambda (n m)
    (= (modulo n m) 0)))


;; How many presents are delivered to a certain house by a specific elf
;
(define delivery-count-by-elf
  (lambda (houseno elfno)
    (if (is-divisible houseno elfno)
      (* 10 elfno)   ; ten times the number of the elf
      0   ; no presents by this elf to this house
      )))


;; How many presents are delivered to this house by all elves in total
;
(define delivery-count-for-house
  (lambda (houseno)
    (let ((numpresents 0))
      (do ((elf 1 (+ elf 1)))
        ((> elf houseno) numpresents)
        (set! numpresents (+ numpresents (delivery-count-by-elf houseno elf)))
        ))))


;; Find the first house to receive at least the required number of presents.
;; Brute-force implementation testing every house in turn.
;; Much too inefficient for large numbers but still works with a trick:
;; Assume that the result will be a multiple of all numbers from 1 to 10
;; because otherwise there will be much less divisors.
;; Not applicable to small numbers, especially not lower than 2520.
;; Returns:  A pair (house-number . number-of-presents)
;
(define at-least-presents-house-wise
  (lambda (n)
    (do ((i 2520 (+ i 2520)))   ; 2520 is divisible by all numbers 1 ... 10
      ((>= (delivery-count-for-house i) n) (cons i (delivery-count-for-house i)))
      )))


;; How many presents are delivered to houses in the range 1 to n
;; factor:  How many presents each elf will deliver per house.
;; limit:   How many houses an elf will deliver presents to at most,
;;          <= 0 for unlimited.
;; Returns:  a hash-table of (house-number . number-of-presents)
;
(define delivery-map
  (lambda (n factor limit)
    (let ((housemap (make-hash-table n)))
      ; All elves
      (do ((elf 1 (+ elf 1)))
          ((> elf n) housemap)
        ; Deliver to all multiples in the house list
        (do ((house elf (+ house elf)))
            ((or (> house n) (and (> limit 0) (> house (* elf limit)))))
          (hashq-set! housemap
                      house
                      (+ (* factor elf)
                         (hashq-ref housemap house 0)))
          )
        ))))


;; Find the first house in the provided house with at least the required
;; number of presents
;; Returns: the first house where the number of presents is high enough.
;; Or abort when no house with enough presents was found;
;; in this case the number of tested houses plus 1 is returned.
;; Returns:  A pair (house-number . number-of-presents)
;
(define find-at-least-presents
  (lambda (n houses)
    (do ((i 1 (+ i 1)))
      ((or (>= (hashq-ref houses i 0) n) (> i n)) (cons i (hashq-ref houses i 0)))
      )))


;; Find the first house to receive at least the required number of presents.
;; Simulate elf by elf and use a hash table to track all houses.
;; Optional arguments:
;; factor:  How many presents each elf will deliver per house, default 10.
;; limit:   How many houses an elf will deliver presents to at most,
;;          <= 0 for unlimited
;; Returns:  A pair (house-number . number-of-presents)
;
(define at-least-presents-elf-wise
  (lambda (n . factor-and-limit)
    (let ((factor (if (> (length factor-and-limit) 0)
                    (list-ref factor-and-limit 0)
                    10))
          (limit (if (> (length factor-and-limit) 1)
                    (list-ref factor-and-limit 1)
                    -1)))
      (find-at-least-presents n (delivery-map
                                  (floor (/ n factor)) factor limit)) )))


;; Read a single item from the specified file
;
(define read-item-from-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (port)
        (read port)))))


(display "----- Examples -----")  (newline)
(do ((i 1 (+ 1 i)))
    ((>= i 10))
  (display "Presents for house ")  (display i)  (display ": ")
  (display (delivery-count-for-house i))  (newline)
  )
(display "Presents for house 4: ")  (display (delivery-count-for-house 4))
  (newline)
(display "At least 60 presents for house: ")
  (display (at-least-presents-elf-wise 60))  (newline)
(display "At least 120 presents for house: ")
  (display (at-least-presents-elf-wise 120))  (newline)
(display "At least 100000 presents for house: ")
  (display (at-least-presents-elf-wise 100000))  (newline)
(newline)

(display "----- Examples with factor 11 and limit 3 houses per elf -----")
  (newline)
; Use a sorted assoc-list because hash-for-each would be in arbitrary order
(for-each
  (lambda (pair)
    (begin
      (display "Presents for house ")  (display (car pair))  (display ": ")
      (display (cdr pair))  (newline)
      ))
  (sort (hash-map->list cons
          (delivery-map 10 11 3)) (lambda (a b) (< (car a) (car b))))
  )
(newline)

(display
  "----- Part 1: First house to receive enough presents -----")
  (newline)
(define minpresents
  (read-item-from-file "20-infinite-elves-infinite-houses-input.txt"))
(display "First house with at least ")
  (display minpresents)
  (display " presents (brute-force/house-wise with trick): ")
  (display (at-least-presents-house-wise 29000000))  (newline)
(display "First house with at least ")  (display minpresents)
  (display " presents (elf-wise):                          ")
  (display (at-least-presents-elf-wise minpresents))  (newline)
(newline)

(display
  "----- Part 2: Limited to 50 houses per elf, delivery of 11 presents each -----")
  (newline)
(display "First house with at least ")  (display minpresents)
  (display " presents (elf-wise):                          ")
  (display (at-least-presents-elf-wise minpresents 11 50))  (newline)
