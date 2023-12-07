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

; 1. Find symbols
; 2. Find immediate numbers around
; 3. Find whole numbers
; 4. Sum

(require racket/file)
(require racket/string)
(require racket/function)
(require racket/set)
(require racket/list)
(require srfi/14)
(require srfi/1)

(define (load-data path)
  (string-split (file->string path) "\n"))
(define input (load-data "data/3.txt"))

(define (number->char n)
  (car (string->list (number->string n))))

(list #\* #\+ #\. #\/ #\space #\0 #\@ #\# #\3 #\$ #\4 #\% #\5 #\& #\6 #\- #\=)

(define numeric-set (list->char-set (map number->char (range 10))))
(define universe-set (list->char-set (string->list (string-join input))))
(define symbol-set (char-set->list (char-set-difference universe-set numeric-set)))

(define minus-set (list->char-set (string->list "-")))
(define (get-prefix symbol-set)
  (list-tail symbol-set (index-of symbol-set #\-)))

(define sorted-symbol-set
  (append* (list (get-prefix symbol-set) (cdr (get-prefix (reverse symbol-set))))))

(define final-symbol-set (filter (lambda (x) (not (eq? x #\.))) sorted-symbol-set))

(define regex
  (regexp (string-append* "" (list "[" (string-append* "" (map string final-symbol-set)) "]"))))

(define (apply-regex str)
  (regexp-match-positions* regex str #:match-select (lambda (x) (caar x))))

(define (apply-regex-num str)
  (regexp-match-positions* #px"[\\d]+" str))
(define tool-locations (map apply-regex input))
(define number-locations (map apply-regex-num input))

(define (pair->list pair)
  (list (car pair) (cdr pair)))

;; Check if tool and number are contiguous
(define (contiguous-tool-range? tool number)
  (ormap (lambda (x) (< (abs (- tool x)) 1)) (pair->list number)))

;; Generate combinations
;; Obtained from https://stackoverflow.com/questions/67954779/pair-combinations-in-scheme
;; Needs further studying
(define (combinations xss)
  (if (null? xss)
      '(())
      (apply append
             (map (lambda (x) (map (lambda (ys) (cons x ys)) (combinations (cdr xss)))) (car xss)))))

;;  Wrapper to expand tuple
(define (wrapper-contiguous-tool-range? tool-numrange)
  (contiguous-tool-range? (car tool-numrange) (cadr tool-numrange)))

;;  Compare tool indices vs number ranges
(define (compare-two-rows tools-row numbers-row)
  (filter wrapper-contiguous-tool-range? (combinations (list tools-row numbers-row))))

(define (compare-tool-to-surroundings tool-adjacents)
  (map (lambda (x)
         (compare-two-rows (car tool-adjacents) x)
         (cadr tool-adjacents))))

;; TODO generate nested arrays (138, 3, ...)
(define all-combinations
  (for/list ([i (range 1 (sub1 (length input)))])
    (combinations (list (list-ref tool-locations i)
                        (for/lists ([j (list (sub1 i) i (add1 i))])
                                   ((list-ref number-locations j)))))))

;; Then apply to each set compare-tool-to-surroundings -> Generate results for all rows
;; Remove duplicate coordinates for contiguous arrays
;; (At last) profit
(combinations (list (cadr tool-locations) (cadr number-locations)))
