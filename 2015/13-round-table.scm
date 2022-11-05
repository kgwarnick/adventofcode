;;; Advent of Code 2015, Day 13 Knights of the Dinner Table
;;; https://adventofcode.com/2015/day/13

(define beispiel-beziehungen '(
  "Alice would gain 54 happiness units by sitting next to Bob."
  "Alice would lose 79 happiness units by sitting next to Carol."
  "Alice would lose 2 happiness units by sitting next to David."
  "Bob would gain 83 happiness units by sitting next to Alice."
  "Bob would lose 7 happiness units by sitting next to Carol."
  "Bob would lose 63 happiness units by sitting next to David."
  "Carol would lose 62 happiness units by sitting next to Alice."
  "Carol would gain 60 happiness units by sitting next to Bob."
  "Carol would gain 55 happiness units by sitting next to David."
  "David would gain 46 happiness units by sitting next to Alice."
  "David would lose 7 happiness units by sitting next to Bob."
  "David would gain 41 happiness units by sitting next to Carol."
  ))


;; Feste Textteile einer Regel kontrollieren
(define check-formulierung
  (lambda (wortliste)
    (and
      (string=? "would"     (cadr wortliste))
      (string=? "happiness" (list-ref wortliste 4))
      (string=? "units"     (list-ref wortliste 5))
      (string=? "by"        (list-ref wortliste 6))
      (string=? "sitting"   (list-ref wortliste 7))
      (string=? "next"      (list-ref wortliste 8))
      (string=? "to"        (list-ref wortliste 9))
      )))


;; Happiness-Wert aus Regel lesen
;; Rückgabewert: Zahl
(define happy-wert
  (lambda (wortliste)
    (cond
      ((string=? (list-ref wortliste 2) "gain") (string->number (list-ref wortliste 3)))
      ((string=? (list-ref wortliste 2) "lose") (- 0 (string->number (list-ref wortliste 3))))
      (else 0))))


;; Konvertiere einzelne als Text vorliegende Regel in Mapping
;; Rückgabewert:  ("Name" . ("Nachbar" . Wert))
(define lese-regel
  (lambda (text)
    (let ((woerter (string-split text #\space)))
      (cond
        ((not (check-formulierung woerter)) (cons (car woerter) (cons "--" 0)))   ; falsche Wörter in Regel
        (else (cons (car woerter)
                    (cons (string-trim-right (list-ref woerter 10) #\.) (happy-wert woerter))
                    ))))))


;; Liste aus zweistufigen Zuordnungen in Tabelle überführen
;; '(                                   '(
;;   (key1 . (key11 . value))    ==>      (key1 . '( (key11 . value11)
;;   (key1 . (key12 . value))                        (key12 . value12) ) )
;;  )                                    )
(define (erzeuge-tabelle liste)
  (let ((tabelle (list)))
    (for-each
      (lambda (outerpair)
        (let ((outerkey (car outerpair))
              (innerpair (cdr outerpair))
              (innerkey (cadr outerpair))
              (innervalue (cddr outerpair))
              (entry (assoc-get-or-create tabelle (car outerpair) (list))))
          (let* ((innerdict (cdr entry))
                 (innerentry (assoc-get-or-create (cdr entry) (cadr outerpair) 0)))
            (set! innerdict (assoc-set! innerdict innerkey (+ (cdr innerentry) (cdr innerpair))))
            (set! tabelle (assoc-set! tabelle outerkey innerdict)))
          ))
      liste)
    tabelle))   ; Neue Tabelle zurückgeben


;; Assoziationslisten-Eintrag suchen oder neuen anlegen
(define assoc-get-or-create
  (lambda (liste key default-value)
    (let ((existing (assoc key liste)))
      (if (not (equal? existing #f))
        (begin existing)
        (begin 
               (cons key default-value)
               ; (assoc-set! liste key default-value)
               )))))


;; Liste zeilenweise numeriert ausgeben
(define (display-list liste)
  (do ((i 0 (+ i 1))
       (a (car liste) (if (> (length d) 0) (car d) '()))
       (d (cdr liste) (if (> (length d) 0) (cdr d) '())))
      ((>= i (length liste)))
    (begin
      (display (string-pad (number->string i) 5))
      (display " ")
      (display a)
      (newline))))


;; Liste ohne doppelte Einträge erzeugen
(define personen-ermitteln
  (lambda (beztab)
    (begin
      (define pers-liste (list)) ;
      (set! pers-liste (list))   ; Start mit leerer Liste
      (for-each
        (lambda (name)
          (if (not (member name pers-liste))                      ; wenn noch nicht enthalten,
            (set! pers-liste (append pers-liste (list name)))))   ; dann hinzufügen
        beztab
        )
      pers-liste   ; gesammelte Liste zurückgeben
    )))


;; Permutationen einer Liste erzeugen
(define permutationen
  (lambda (eingliste)
    (let ((perms (list)))
      (cond
        ((null? eingliste) (list (list)))   ; leere Ausgabe für leere Eingabe
        (else
          (for-each
            (lambda (name) (begin   ; (display "* ") (display eingliste) (newline)
              (for-each
                (lambda (teilperm)
                  (begin   ; (display "-") (display teilperm) (newline)
                    (set! perms (append perms (list (append (list name) teilperm))))
                    ; (display "[") (display name) (display (delete name eingliste)) (display "]")
                    ; (display " :: ") (display perms) (newline)
                  ))
                (permutationen (delete name eingliste))))   ; Permutationen der Rest-Liste
              )
            eingliste)
          perms))
          )))


;; reduce-Funktion
(define list-reduce
  (lambda (func start liste)
    (if (null? liste)
      start
    ; (func (car liste) (list-reduce func start (cdr liste)))
      (list-reduce func (func start (car liste)) (cdr liste))
      )))


;; Happiness-Wert für als Liste übergebene Sitzordnung bestimmen
;; Rückgabewert:  Ein summierter Wert
(define sitzordnung-happiness
  (lambda (sitzordnung regeln)
    (list-reduce + 0 (personen-happiness sitzordnung regeln))))


;; Liste der Happiness-Wert für einzelne Personen
;; Rückgabewert:  Liste von Enzelwerten
(define personen-happiness
  (lambda (sitzordnung regeln)
    (let ((happinesses (list)))
      (do ((i 0 (+ i 1)))
        ((>= i (length sitzordnung)) happinesses)
        (set! happinesses (append happinesses (list
          (person-happiness
            regeln
            (list-ref sitzordnung i)   ; Name der Person selbst
            (list-ref-ring sitzordnung (- i 1)))   ; Name des linken Nachbars
          (person-happiness
            regeln
            (list-ref sitzordnung i)
            (list-ref-ring sitzordnung (+ i 1)))   ; Name des rechten Nachbars
          )))
        ))))


;; Happiness-Wert für eine Person mit einem bestimmten Nachbarn
;; Rückgabewert: Zahl
(define person-happiness
  (lambda (regeltabelle person neighbour)
    (assoc-ref
      (assoc-ref regeltabelle person)
      neighbour)))


;; Listenelement zurückgeben bei Zählung im Kreis über den/das Listenanfang/-ende hinaus
(define list-ref-ring
  (lambda (liste index)
    (list-ref liste (modulo index (length liste)))))


;; Tests für Hilfs-Funktionen

; (display "Reduce-Test: ") (display (list-reduce + 0 '(1 2 3))) (newline)
; (display "list-ref-ring: ") (display (list-ref-ring '(10 20 30 40) 5)) (newline)
; (display "list-ref-ring: ") (display (list-ref-ring '(10 20 30 40) -1)) (newline)

; (display (assoc-get-or-create '(("a" . ("a1" . 1)) ("a" . ("a2" . 2))) "a" '())) (newline)
; (display (erzeuge-tabelle '(("a" . ("a1" . 1)) ("a" . ("a2" . 2)) ("a" . ("a3" . 3))))) (newline)

(display "--- Beispiel ---") (newline)

(display (length beispiel-beziehungen)) (display " Regeln") (newline)
; (display-list beispiel-beziehungen)
(define beztab (map lese-regel beispiel-beziehungen))
(display-list beztab)
(define happymap (erzeuge-tabelle (map lese-regel beispiel-beziehungen)))
(display "Happiness-Tabelle:") (newline) (display-list happymap)

(define bsppersonen (personen-ermitteln (map car beztab)))
(display "Personen: ") (display bsppersonen) (newline)
(display "Permutationen: ") (display (length (permutationen bsppersonen))) (newline)
(display-list (permutationen bsppersonen))


(display "Alice neben Bob:   ") (display (person-happiness happymap "Alice" "Bob")) (newline)
(display "Alice neben Carol: ") (display (person-happiness happymap "Alice" "Carol")) (newline)
(display "Bob neben Alice:   ") (display (person-happiness happymap "Bob" "Alice")) (newline)
(display "Alice, Bob, Carol (Einzelwerte): ")
(display (personen-happiness '("Alice" "Bob" "Carol") happymap)) (newline)
(display "Alice, Bob, Carol (Summe):       ")
(display (sitzordnung-happiness '("Alice" "Bob" "Carol") happymap)) (newline)

;; Beispiel-Sitzordnung
(display "Happiness für Sitzordung ") (display bsppersonen) (display ": ")
(display (sitzordnung-happiness bsppersonen happymap)) (newline)

(define bsp-happinesses
  (map
    (lambda (arr)
      (cons arr (sitzordnung-happiness arr happymap)))
    (permutationen bsppersonen)))
(display "Mögliche Happiness-Ergebnisse der Beispiel-Gästeliste: ") (newline)
; (display-list bsp-happinesses)
(display-list (sort bsp-happinesses (lambda (a b) (> (cdr a) (cdr b)))))
(newline)


;; Aufgabe 1: Opimale Sitzordnung für alle acht Personen

(display "--- Aufgabe 1 ---") (newline)

(define happy-relations '(
  "Alice would gain 2 happiness units by sitting next to Bob."
  "Alice would gain 26 happiness units by sitting next to Carol."
  "Alice would lose 82 happiness units by sitting next to David."
  "Alice would lose 75 happiness units by sitting next to Eric."
  "Alice would gain 42 happiness units by sitting next to Frank."
  "Alice would gain 38 happiness units by sitting next to George."
  "Alice would gain 39 happiness units by sitting next to Mallory."
  "Bob would gain 40 happiness units by sitting next to Alice."
  "Bob would lose 61 happiness units by sitting next to Carol."
  "Bob would lose 15 happiness units by sitting next to David."
  "Bob would gain 63 happiness units by sitting next to Eric."
  "Bob would gain 41 happiness units by sitting next to Frank."
  "Bob would gain 30 happiness units by sitting next to George."
  "Bob would gain 87 happiness units by sitting next to Mallory."
  "Carol would lose 35 happiness units by sitting next to Alice."
  "Carol would lose 99 happiness units by sitting next to Bob."
  "Carol would lose 51 happiness units by sitting next to David."
  "Carol would gain 95 happiness units by sitting next to Eric."
  "Carol would gain 90 happiness units by sitting next to Frank."
  "Carol would lose 16 happiness units by sitting next to George."
  "Carol would gain 94 happiness units by sitting next to Mallory."
  "David would gain 36 happiness units by sitting next to Alice."
  "David would lose 18 happiness units by sitting next to Bob."
  "David would lose 65 happiness units by sitting next to Carol."
  "David would lose 18 happiness units by sitting next to Eric."
  "David would lose 22 happiness units by sitting next to Frank."
  "David would gain 2 happiness units by sitting next to George."
  "David would gain 42 happiness units by sitting next to Mallory."
  "Eric would lose 65 happiness units by sitting next to Alice."
  "Eric would gain 24 happiness units by sitting next to Bob."
  "Eric would gain 100 happiness units by sitting next to Carol."
  "Eric would gain 51 happiness units by sitting next to David."
  "Eric would gain 21 happiness units by sitting next to Frank."
  "Eric would gain 55 happiness units by sitting next to George."
  "Eric would lose 44 happiness units by sitting next to Mallory."
  "Frank would lose 48 happiness units by sitting next to Alice."
  "Frank would gain 91 happiness units by sitting next to Bob."
  "Frank would gain 8 happiness units by sitting next to Carol."
  "Frank would lose 66 happiness units by sitting next to David."
  "Frank would gain 97 happiness units by sitting next to Eric."
  "Frank would lose 9 happiness units by sitting next to George."
  "Frank would lose 92 happiness units by sitting next to Mallory."
  "George would lose 44 happiness units by sitting next to Alice."
  "George would lose 25 happiness units by sitting next to Bob."
  "George would gain 17 happiness units by sitting next to Carol."
  "George would gain 92 happiness units by sitting next to David."
  "George would lose 92 happiness units by sitting next to Eric."
  "George would gain 18 happiness units by sitting next to Frank."
  "George would gain 97 happiness units by sitting next to Mallory."
  "Mallory would gain 92 happiness units by sitting next to Alice."
  "Mallory would lose 96 happiness units by sitting next to Bob."
  "Mallory would lose 51 happiness units by sitting next to Carol."
  "Mallory would lose 81 happiness units by sitting next to David."
  "Mallory would gain 31 happiness units by sitting next to Eric."
  "Mallory would lose 73 happiness units by sitting next to Frank."
  "Mallory would lose 89 happiness units by sitting next to George."
  ))

(define happy-rules (map lese-regel happy-relations))
(define happy-table (erzeuge-tabelle (map lese-regel happy-relations)))
(define gästeliste (personen-ermitteln (map car happy-rules)))
(display "Personen: ") (display gästeliste) (newline)
(newline)
(display "Happiness table: ") (newline) (display-list happy-table)
(define arrangements (permutationen gästeliste))
(display "Mögliche Sitzordnungen: ") (display (length arrangements)) (newline) ; (display-list arrangements)

(define happiness-arrangements
  (map
    (lambda (arr)
      (cons arr (sitzordnung-happiness arr happy-table)))
    arrangements))
(display "Achievable happiness for different arrangements: ") (newline)
; (display-list happiness-arrangements)
(define sorted-happiness (sort happiness-arrangements (lambda (a b) (< (cdr a) (cdr b)))))
(display-list sorted-happiness)
