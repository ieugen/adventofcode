(define-module (an2023 a02-cube-conundrum)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 textual-ports))


;; --- Day 2: Cube Conundrum ---

;; You're launched high into the atmosphere! The apex of your trajectory just
;; barely reaches the surface of a large island floating in the sky.
;; You gently land in a fluffy pile of leaves.
;; It's quite cold, but you don't see much snow. An Elf runs over to greet you.

;; The Elf explains that you've arrived at Snow Island and apologizes for the lack
;; of snow. He'll be happy to explain the situation, but it's a bit of a walk,
;; so you have some time. They don't get many visitors up here; would you like
;; to play a game in the meantime?

;; As you walk, the Elf shows you a small bag and some cubes which are
;; either red, green, or blue. Each time you play this game, he will hide a secret
;; number of cubes of each color in the bag, and your goal is to figure out information
;; about the number of cubes.

;; To get information, once a bag has been loaded with cubes, the Elf will reach into
;; the bag, grab a handful of random cubes, show them to you, and then put them back
;; in the bag. He'll do this a few times per game.

;; You play several games and record the information from each game (your puzzle input).
;; Each game is listed with its ID number (like the 11 in Game 11: ...) followed by a
;; semicolon-separated list of subsets of cubes that were revealed from the bag
;; (like 3 red, 5 green, 4 blue).

;; For example, the record of a few games might look like this:

;; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
;; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
;; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
;; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
;; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

;; In game 1, three sets of cubes are revealed from the bag (and then put back again).
;; The first set is 3 blue cubes and 4 red cubes; the second set is 1 red cube,
;; 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.

;; The Elf would first like to know which games would have been possible if the bag
;; contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

;; In the example above, games 1, 2, and 5 would have been possible if the bag had been
;; loaded with that configuration. However, game 3 would have been impossible
;; because at one point the Elf showed you 20 red cubes at once; similarly,
;; game 4 would also have been impossible because the Elf showed you 15
;; blue cubes at once. If you add up the IDs of the games that would have been
;; possible, you get 8.

;; Determine which games would have been possible if the bag had been loaded with
;; only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs
;; of those games?

;; --- Part Two ---

;; The Elf says they've stopped producing snow because they aren't getting any water! He isn't sure why the water stopped; however, he can show you how to get to the water source to check it out for yourself. It's just up ahead!

;; As you continue your walk, the Elf poses a second question: in each game you played, what is the fewest number of cubes of each color that could have been in the bag to make the game possible?

;; Again consider the example games from earlier:

;; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
;; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
;; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
;; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
;; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

;;     In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If any color had even one fewer cube, the game would have been impossible.
;;     Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
;;     Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
;;     Game 4 required at least 14 red, 3 green, and 15 blue cubes.
;;     Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.

;; The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together. The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these five powers produces the sum 2286.

;; For each game, find the minimum set of cubes that must have been present. What is the sum of the power of these sets?

(define cube1
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
  Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
  Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
  Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    ")

;; Datele de intrare sunt textul cu probleme și configurație de cuburi
;; Citim fiecare joc și validăm dacă jocul este posibil.
;; Jocul este posibil dacă fiecare set are culorile mai mici sau egale decât culorile
;; din cnfigurație.
;; Pentru fiecare joc posibil adunăm numărul jocului la sumă.
;; Când am rămas fără jocuri, întoarcem suma.
;; O linie de joc o procesăm așa:
;; - Ștergem 'Game ' din string - dacă este prezent - altfel eroare
;; - Citim ID joc (până la :) și convertim la număr
;; - Citim seturile de cuburi până la punct și virgulă ';'
;; - Dacă jocul este posibil continuăskm cu următorul set
;; - Dacă jocul nu este posibil trecem la linia următoare
;; - Dacă este ultimul set din joc și este ok, jocul este bun
;; - Dacă jocul este bun, adunăm ID joc la sumă și trecem la jocul următor.

(define (game-id game-str)
  (string->number
   (second (string-split
            (string-trim-both game-str)
            #\space))))

(define-record-type <set-of-cubes>
  (make-set-of-cubes red green blue)
  set-of-cubes?
  (red get-red set-red!)
  (green get-green set-green!)
  (blue get-blue set-blue!))

#|

(string-split
(second (string-split "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" #\:))
#\;)

(string-split "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" #\:)

(game-id "Game 1")

(define cubes-set-1 (make-set-of-cubes 91 3 5))

(cubes-red cubes-set-1)
|#

(define (set-cubes-color! my-set-of-cubes cubes+color)
  ;; Set the cube count for the specific color in a set-of-cubes record
  ;; one of ("3" "blue") ("4" "red") ("2" "green")
  (let* ((cubes (string->number (first cubes+color)))
         (color (second cubes+color)))
    (cond
     ((string=? color "red")
      (set-red! my-set-of-cubes cubes)
      my-set-of-cubes)
     ((string=? color "blue")
      (set-blue! my-set-of-cubes cubes)
      my-set-of-cubes)
     ((string=? color "green")
      (set-green! my-set-of-cubes cubes)
      my-set-of-cubes)
     (else my-set-of-cubes))))

#|

(set-cubes-color! (make-set-of-cubes 0 0 0) '("3" "blue"))

(set-cubes-color! (make-set-of-cubes 0 0 0) '("3" "red"))

(set-cubes-color! (make-set-of-cubes 0 0 0) '("3" "green"))

(set-cubes-color! (make-set-of-cubes 0 0 0) '("3" "aa"))

|#

(define (cubes-list->set-of-cubes cubes-list)
  ;; Convert a list of cubes: (("3" "blue") ("4" "red") ("2" "green"))
  ;; to a set-of-cubes record
  (let* ((my-cubes (make-set-of-cubes 0 0 0)))
    (for-each
     (lambda (x) (set-cubes-color! my-cubes x))
     cubes-list)
    my-cubes))

#|

(cubes-list->set-of-cubes '(("3" "blue") ("4" "red") ("2" "green")))

(cubes-list->set-of-cubes '(("3" "blue") ("4" "red"))

(cubes-list->set-of-cubes '(("3" "blue") ("2" "green")))

(cubes-list->set-of-cubes '(("4" "red") ("2" "green")))

|#


(define (game-str->set-of-cubes game-set-str)
  ;; Convert a string representation of a game into a set-of-cubes record
  ;; input: " 3 blue, 4 red"
  ;; output: <set-of-cubes>
  (let* ((split-xform (compose (tmap string-trim-both)
                               (tmap (lambda (x) (string-split x #\space)))))
         ;; split cubes to => (("3" "blue") ("4" "red") ("2" "green"))
         (games (list-transduce split-xform
                                rcons
                                (string-split game-set-str #\,))))
    (cubes-list->set-of-cubes games)))

(define (str->game-sets game-sets-str)
  (let* ((game-sets (string-split game-sets-str #\;)))
    (map game-str->set-of-cubes game-sets)))

#|

(game-str->set-of-cubes " 3 blue, 4 red, 2 green")

(define my-games " 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
(str->game-sets my-games)

|#

(define-record-type <game>
  (make-game id cube-sets)
  game?
  (id get-game-id set-game-id!)
  (cube-sets get-cube-sets set-cube-sets!))

(define (game-line-str->game game-line)
  (let* ((gl (string-split game-line #\:))
         (id (game-id (first gl)))
         (game-sets (str->game-sets (second gl))))
    ;; (format #t "\t id: ~a gl: ~a" id gl)
    (make-game id game-sets)))

#|
(game-line-str->game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

|#

(define (valid-game? elf-bag game)
  ;; test if the game is valid by checking the max number of cubes in elf-bag
  (and
   (>= (get-red elf-bag) (get-red game))
   (>= (get-blue elf-bag) (get-blue game))
   (>= (get-green elf-bag) (get-green game))))

#|

(valid-game? (make-set-of-cubes 12 13 14) (make-set-of-cubes 1 3 0))
(valid-game? (make-set-of-cubes 12 13 14) (make-set-of-cubes 12 13 14))

(valid-game? (make-set-of-cubes 12 13 14) (make-set-of-cubes 13 0 0))
(valid-game? (make-set-of-cubes 12 13 14) (make-set-of-cubes 0 14 0))
(valid-game? (make-set-of-cubes 12 13 14) (make-set-of-cubes 0 0 15))

(< 0 1)


|#

(define elf-bag1 (make-set-of-cubes 12 13 14))

(define (game->min-set-of-cubes game-sets)
  ;; Compute the minimum set of cubes that the game can be played with
  ;; Compute max of each cube type per game
  (let* ((cubes-count (make-set-of-cubes 0 0 0)))
    (for-each
     (lambda (c)
       ;; (format #t "~a ~a" c cubes-count)
       (when (< (get-blue cubes-count)
                (get-blue c))
         (set-blue! cubes-count (get-blue c)))
       (when (< (get-red cubes-count)
                (get-red c))
         (set-red! cubes-count (get-red c)))
       (when (< (get-green cubes-count)
                (get-green c))
         (set-green! cubes-count (get-green c))))
     game-sets)
    cubes-count))

(define (ignore-zero cube-count)
  ;; In case cub-count is zero, reuturn 1 which is neutral in multiplication
  (if (zero? cube-count)
      1
      cube-count))

(define (cube-power cube-set)
  (let* ((red (ignore-zero (get-red cube-set)))
         (green (ignore-zero (get-green cube-set)))
         (blue (ignore-zero (get-blue cube-set))))
    (* red green blue)))

#|

(game->min-set-of-cubes (list (make-set-of-cubes 4 0 3) (make-set-of-cubes 1 2 6) (make-set-of-cubes 0 2 0) ))

(ignore-zero 99)
(ignore-zero 0)

(cube-power (make-set-of-cubes 20 13 6))
(cube-power (make-set-of-cubes 20 13 0))


|#

(define (solve-cube port elf-bag)
  ;; given a string port for game data and an elf bag of type set-of-cubes
  ;; Compute the sum of valid game ids
  (set-port-encoding! port "UTF-8")
  (let joaca ((suma 0)
              (power 0)
              (ln (get-line port)))
    ;; (format #t "procesam: ~a ~a '~a'\n" suma power ln)
    (if (or (eof-object? ln)
            (string-null? (string-trim-both ln)))
        ;; Jocul s-a terminat, trebuie să întoarcem suma
        (let ((sol (cons suma power)))
          ;; (format #t "Solution ~a" sol)
          sol)
        ;; avem un joc nou pe care să îl procesăm
        (let* ((game (game-line-str->game ln))
               (min-cubes (game->min-set-of-cubes (get-cube-sets game)))
               (game-powa (cube-power min-cubes))
               (total-powa (+ power game-powa))
               (invalid-games (filter
                               (lambda (g) (not (valid-game? elf-bag g)))
                               (get-cube-sets game)))
               (valid? (zero? (length invalid-games)))
               (next-line (get-line port)))
          ;; (format #t "\tjoc: ~a ~a ~a ~a\n" valid? game-powa total-powa game)
          (if valid?
              (joaca (+ suma (get-game-id game)) total-powa next-line)
              (joaca suma total-powa next-line))))))


(call-with-input-string cube1 (lambda (x) (solve-cube x elf-bag1)))

#|

(call-with-input-string cube1 (lambda (x) (solve-cube x elf-bag1)))

(call-with-input-file "an2023/a02-cube-conundrum-01.txt" (lambda (x) (solve-cube x elf-bag1)))


(format #t "Process ~s" "aa")

|#
