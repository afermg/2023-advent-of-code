;; --- Day 3: Gear Ratios ---

;; You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.

;; It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

;; "Aaah!"

;; You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.

;; The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.

;; The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)

;; Here is an example engine schematic:

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..

;; In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

;; Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

(require racket/file)
(require racket/string)
(require racket/set)
(require racket/list)
(require srfi/14)

(define (load-data path)
  (string-split (file->string path) "\n"))

(define (number->char n)
  (car (string->list (number->string n))))

(define (get-prefix symbol-set)
  (list-tail symbol-set (index-of symbol-set #\-)))

(define (apply-regex str)
  (regexp-match-positions* regex str #:match-select caar))

(define (cons->list rng)
  (list (car rng) (cdr rng)))

(define (num-range-contiguous? num rng)
  (ormap (lambda (x) (<= (abs (- num x)) 1)) (cons->list rng)))

(define (filter-relevant-numbers tool-row number-row)
  ;; Select the numbers adjacent to a tool
  (filter (lambda (x) (ormap (lambda (y) (num-range-contiguous? y x)) tool-row)) number-row))

(define (get-valid-numbers-from-rows tool-row number-rows)
  (map (lambda (x) (filter-relevant-numbers tool-row x)) number-rows))

(define (get-valid-unsorted tool-coords-matrix number-rows-matrix)
  (append* (for/list ([index (range (length tool-coords-matrix))])
             (get-valid-numbers-from-rows (list-ref tool-coords-matrix index)
                                          (map (lambda (i) (list-ref number-coords-matrix i))
                                               (range (max 0 (- index 1))
                                                      (min (length input) (+ index 2))))))))

(define (get-edge lst)
  (list (append* (list (car lst) (caddr lst)))))

(define (get-valid-sorted lst)
  (append* (list (get-edge lst)
                 (for/list ([i (range 1 (- (length lst) 5) 3)])
                   (append* (map (lambda (x) (list-ref lst x)) (range i (+ i 5) 2))))
                 (get-edge (reverse lst)))))

(define (substring->num str rng)
  ;; Convert string and range (as cons) to numeric
  (string->number (substring str (car rng) (cdr rng))))

(define input (load-data "data/3.txt"))
;; (define input (load-data "data/3_2.txt"))
(define numeric-set (list->char-set (map number->char (range 10))))
(define universe-set (list->char-set (string->list (string-replace (string-join input) " " ""))))
(define symbol-set (char-set->list (char-set-difference universe-set numeric-set)))
(define sorted-symbol-set
  (if (member #\- symbol-set)
      (append (get-prefix symbol-set) (cdr (get-prefix (reverse symbol-set))))
      symbol-set))
(define regex
  (regexp (string-replace
           (string-append* "" (list "[" (string-append* "" (map string sorted-symbol-set)) "]"))
           "."
           "")))
(define tool-coords-matrix (map apply-regex input))
;; Note that car and (reverse car) for tool-coords-matrix are empty, this simplifies things
(define number-coords-matrix (map (lambda (x) (regexp-match-positions* #rx"[0-9]+" x)) input))
(define valid-unsorted (get-valid-unsorted tool-coords-matrix number-coords-matrix))
(define valid-sorted (get-valid-sorted valid-unsorted))
(define valid-sets (map list->set valid-sorted))

(define numbers-by-tools
  (for/list ([row input] [valid-coords valid-sets])
    (map (lambda (x) (substring->num row x)) (set->list valid-coords))))

(define result-1 (apply + (append* numbers-by-tools)))
