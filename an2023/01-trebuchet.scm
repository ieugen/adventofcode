(define-module (an2023 01-trebuchet)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports))

;;
;; https://adventofcode.com/2023/day/1
;;

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


;;
;; Pornesc de la primul caracater, prima linie.
;; Suma = 0, prima cifră = nil, cifră curentă = nil
;; La primul caracter cifră, îl rețin.
;; Merg și rețin cifra nouă până la linia nouă.
;; Când ajung la linia nouă, iau prima și curenta, fac un număr și adaug la sumă.
;; Dacă nu este curenta, dublez prima literă.
;; Dacă nu este nici prima - trec mai departe.
;; Resetez prima și cifra curentă.
;; Când ajung la final tipăresc suma.
;;

(define puzzle-map "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
")

(string-length puzzle-map)

(define val-of-0 (char->integer #\0))

(define (char->digit c)
  (- (char->integer c) val-of-0))

(define (calibration-score calibration-doc)
  (let calibrate ((x 0)
                  (sum 0)
                  (n1 #nil)
                  (n2 #nil))
    (if (< x (string-length calibration-doc))
        (letrec* ((cc (string-ref calibration-doc x))
                  (next-idx (+ x 1))
                  (is-digit? (char-numeric? cc))
                  (n-line? (char=? cc #\newline)))
          (if n-line?
              (if (not n2)
                  (if (not n1)
                      (calibrate next-idx sum #nil #nil)
                      (calibrate next-idx (+ sum (* 10 n1) n1) #nil #nil))
                  (calibrate next-idx (+ sum (* 10 n1) n2) #nil #nil))
              (if is-digit?
                  (if n1
                      (calibrate next-idx sum n1 (char->digit cc))
                      (calibrate next-idx sum (char->digit cc) n2))
                  (calibrate next-idx sum n1 n2))))
        sum)))

(let ((a #\0))
  (and
   (char>=? a #\0 )
   (char<=? a #\9)))

(char=? (string-ref "
" 0) #\newline)

(char->integer #\0)

(calibration-score puzzle-map)

;; part 1
(calibration-score (call-with-input-file "an2023/01-trebuchet-input-01.txt"
                     get-string-all))


;; part 2
;;
;; --- Part Two ---
;; Your calculation isn't quite right.
;; It looks like some of the digits are actually spelled out with letters:
;; one, two, three, four, five, six, seven, eight, and nine also count as
;; valid "digits".
;; Equipped with this new information, you now need to find the real
;; first and last digit on each line.
;; For example:

;; two1nine
;; eightwothree
;; abcone2threexyz
;; xtwone3four
;; 4nineeightseven2
;; zoneight234
;; 7pqrstsixteen

;; In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.
;;


;;
;; Algoritmul este similar cu cel inițial.
;; Doar că acum verificăm caracterul curent daca este început
;; de număr cuvânt.
;; Dacă este verificăm dacă urmează un număr cuvânt.
;; Folosim string-containes cu start și end index pentru eficiență.
;; Dacă da, încercăm să punem n1 sau n2 și creștem indexul
;; cu mărimea cuvântului să nu mai procesăm inutil.
;; Dacă nu este creștem indexul cu 1.
;;

;;
;; Check if we have a digit as a word in the document.
;; Start at current-idx and current char.
;; Return #f if not.
;; If it's a digit, it returns a pair of digit . chars to skip
(define (is-word-digit? doc current-idx cc)
  ;;(format #t "~a . ~a\n" current-idx cc)
  (letrec* ((dlen (string-length doc))
            (remaining (- dlen current-idx)))
    (cond
     ((char=? cc #\o)
      ;; check "one"
      (if (string-contains doc "one" current-idx
                           (+ current-idx (min (string-length "one")
                                               remaining)))
          '(1 . 3)
          #f))
     ((char=? cc #\t)
      ;; check two or three
      (if (string-contains doc "two" current-idx
                           (+ current-idx (min (string-length "two")
                                               remaining)))
          '(2 . 3)
          (if (string-contains doc "three" current-idx
                               (+ current-idx (min (string-length "three")
                                                   remaining)))
              '(3 . 5)
              #f)))
     ((char=? cc #\f)
      ;; check four, five
      (if (string-contains doc "four" current-idx
                           (+ current-idx (min (string-length "four")
                                               remaining)))
          '(4 . 4)
          (if (string-contains doc "five" current-idx
                               (+ current-idx (min (string-length "five")
                                                   remaining)))
              '(5 . 4)
              #f)))
     ((char=? cc #\s)
      ;; check six, seven
      (if (string-contains doc "six" current-idx
                           (+ current-idx (min (string-length "six")
                                               remaining)))
          '(6 . 3)
          (if (string-contains doc "seven" current-idx
                               (+ current-idx (min (string-length "seven")
                                                   remaining)))
              '(7 . 5)
              #f)))
     ((char=? cc #\e)
      ;; check eight
      (if (string-contains doc "eight" current-idx
                           (+ current-idx (min (string-length "eight")
                                               remaining)))
          '(8 . 5)
          #f))
     ((char=? cc #\n)
      ;; check nine
      (if (string-contains doc "nine" current-idx
                           (+ current-idx (min (string-length "nine")
                                               remaining)))
          '(9 . 4)
          #f))
     (else #f))))


(define puzzle-map2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
")

(string-length puzzle-map2)


(string-contains puzzle-map2 "one" 26 30)

(is-word-digit? puzzle-map2 0 (string-ref puzzle-map2 0))

(car '(1 . 3))

(cdr '(1 . 3))

(define (calibration-score-words calibration-doc)
  (let calibrate ((x 0)
                  (sum 0)
                  (n1 #nil)
                  (n2 #nil))
    (if (< x (string-length calibration-doc))
        (letrec* ((cc (string-ref calibration-doc x))
                  (is-digit? (char-numeric? cc))
                  (word-digit (is-word-digit? calibration-doc x cc))
                  (next-idx (if word-digit
                                (+ x (cdr word-digit))
                                (+ x 1)))
                  (digit (if is-digit?
                             (char->digit cc)
                             (if word-digit
                                 (car word-digit)
                                 #f))))
          (if (char=? cc #\newline)
              (if (not n2)
                  (if (not n1)
                      (calibrate next-idx sum #nil #nil)
                      (letrec* ((line-sum (+ (* n1 10) n1))
                                (sum2 (+ sum line-sum)))
                        (format #t "Line sum (~a): ~a~a = ~a (~a)\n"
                                sum n1 n1 sum2 x)
                        (calibrate next-idx sum2 #nil #nil)))
                  (letrec* ((line-sum (+ (* n1 10) n2))
                            (sum2 (+ line-sum sum)))
                    (format #t "Line sum (~a): ~a~a = ~a (~a)\n"
                            sum n1 n2 sum2 x)
                    (calibrate next-idx sum2 #nil #nil)))
              (if digit
                  (if n1
                      (calibrate next-idx sum n1 digit)
                      (calibrate next-idx sum digit n2))
                  (calibrate next-idx sum n1 n2))))
        sum)))

(calibration-score-words puzzle-map2)

(calibration-score-words (call-with-input-file "an2023/01-trebuchet-input-01.txt"
                           get-string-all))
