;; --- Day 1: Trebuchet?! ---

;; Something is wrong with global snow production, and you've been selected to take a look. The Elves have even given you a map; on it, they've used stars to mark the top fifty locations that are likely to be having problems.

;; You've been doing this long enough to know that to restore snow operations, you need to check all fifty stars by December 25th.

;; Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

;; You try to ask why they can't just use a weather machine ("not powerful enough") and where they're even sending you ("the sky") and why your map looks mostly blank ("you sure ask a lot of questions") and hang on did you just say the sky ("of course, where do you think snow comes from") when you realize that the Elves are already loading you into a trebuchet ("please hold still, we need to strap you in").

;; As they're making the final adjustments, they discover that their calibration document (your puzzle input) has been amended by a very young Elf who was apparently just excited to show off her art skills. Consequently, the Elves are having trouble reading the values on the document.

;; The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.

;; For example:

;; 1abc2
;; pqr3stu8vwx
;; a1b2c3d4e5f
;; treb7uchet

;; In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.

;; Consider your entire calibration document. What is the sum of all of the calibration values?

(require racket/file)
(require racket/string)

(define (load-data path)
  (string-split (file->string path) "\n"))
(define input (load-data "data/1.txt"))

(define (first-numeric str)
  (findf char-numeric? (string->list str)))
(define (first-and-last str)
  (list (first-numeric str) (first-numeric (list->string (reverse (string->list str))))))
(define (concat-first-last-numeric str)
  (string->number (list->string (first-and-last str))))

;; Apply to vector
(define result-1 (apply + (map concat-first-last-numeric input)))

;; --- Part Two ---

;; Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".

;; Equipped with this new information, you now need to find the real first and last digit on each line. For example:

;; two1nine
;; eightwothree
;; abcone2threexyz
;; xtwone3four
;; 4nineeightseven2
;; zoneight234
;; 7pqrstsixteen

;; In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.

;; What is the sum of all of the calibration values?

;; Parse numbers and letters as occurrrences
(require racket/list)
(require racket/dict)
(require srfi/13)

(define (reverse-string str)
  (list->string (reverse (string->list str))))
(define numbers (range 10))
(define numbers-str (map number->string numbers))
(define numbers-names
  (append '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine") numbers-str))
(define numbers-names-reverse (map reverse-string numbers-names))

(define dict-values
  (apply append (make-list 2 (map (lambda (x) (car (string->list x))) numbers-str))))

(define cons-forward (map cons (append numbers-names) dict-values))
(define cons-backward (map cons (append numbers-names-reverse) dict-values))

(define (first-occurrence str cons-direction)
  (cdr (assoc (argmin (lambda (n)
                        (let ([result (string-contains str n)])
                          (if (real? result) result (string-length str))))
                      (dict-keys cons-direction))
              cons-direction)))

(define (first-last str)
  (string->number (list->string (list (first-occurrence str cons-forward)
                                      (first-occurrence (reverse-string str) cons-backward)))))

(define result-2 (apply + (map first-last input)))
