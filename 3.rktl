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
(require racket/set)
(require racket/list)

(define (load-data path)
  (string-split (file->string path) "\n"))
(define input (load-data "data/3.txt"))

(define (number->char n)
  (car (string->list (number->string n))))

(list #\* #\+ #\. #\/ #\space #\0 #\@ #\# #\3 #\$ #\4 #\% #\5 #\& #\6 #\- #\=)

(require srfi/14)
(define numeric-set (list->char-set (map number->char (range 10))))
(define universe-set (list->char-set (string->list (string-join input))))
(define symbol-set (char-set->list (char-set-difference universe-set numeric-set)))

(define regex (regexp (string-append* "" (list "[" (string-append* "" (map string symbol-set)) "]"))))

(define (apply-regex str)
  (regexp-match-positions* regex str))

(define occur-matrix (map apply-regex input))

(define minus-set (list->char-set (string->list "-")))
(define (get-prefix symbol-set)
  (list-tail symbol-set (index-of symbol-set #\-)))
