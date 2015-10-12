#| Assignment 1 - Functional Shakespeare Interpreter

Read through the starter code carefully. In particular, look for:

- interpret: the main function used to drive the program.
  This is provided for you, and should not be changed.
- evaluate: this is the main function you'll need to change.
  Please put all helper functions you write below this one.
  Doing so will greatly help TAs when they are marking. :)
|#
#lang racket

; You are allowed to use all the string functions in this module.
; You may *not* import any other modules for this assignment.
(require racket/string)

; This exports the main driver function. Used for testing purposes.
; This is the only function you should export. Don't change this line!
(provide interpret)

;------------------------------------------------------------------------------
; Parsing constants
;------------------------------------------------------------------------------

; Sections dividers
(define personae "Dramatis personae")
(define settings "Settings")
(define finis "Finis")

; Comment lines
(define comments '("Act" "Scene"))

; List of all "bad words" in a definition
(define bad-words
  '("vile"
    "villainous"
    "wicked"
    "naughty"
    "blackhearted"
    "shameless"
    "scoundrelous"))

; Arithmetic
(define add "join'd with")
(define mult "entranc'd by")

; Self-reference keywords
(define self-refs
  '("I"
    "me"
    "Me"
    "myself"
    "Myself"))

; Function call
(define call "The song of")

; Function parameter name
(define param "Hamlet")

;------------------------------------------------------------------------------
; Interpreter driver
;------------------------------------------------------------------------------

#|
(interpret filename)
  filename: a string representing the path to a FunShake file

  Returns a list of numbers produced when evaluating the FunShake file.
  You can complete this assignment without modifying this function at all,
  but you may change the implementation if you like. Please note that you may
  not change the interface, as this is the function that will be autotested.
|#
(define (interpret filename)
  (let* ([contents (port->string (open-input-file filename))]
         [lines (map normalize-line (string-split contents "\n"))]
         ; Ignore title, empty, and comment lines
         [body (remove-empty-and-comments (rest lines))])
    (evaluate body)))

#|
(normalize-line str)
  str: the line string to normalize

  Remove trailing period and whitespace.
|#
(define (normalize-line str)
  (string-trim (string-normalize-spaces (string-trim str)) "."))

#|
(remove-empty-and-comments strings)
  strings: a list of strings

  Removes all empty strings and FunShake comment strings from 'strings'.
|#
(define (remove-empty-and-comments strings)
  (filter (lambda (s)
            (and
             (< 0 (string-length s))
             (not (ormap (lambda (comment) (prefix? comment s))
                         comments))))
          strings))

#|
(prefix? s1 s2)
  s1, s2: strings

  Returns whether 's1' is a prefix of 's2'.
|#
(define (prefix? s1 s2)
  (and (<= (string-length s1) (string-length s2))
       (equal? s1 (substring s2 0 (string-length s1)))))

;------------------------------------------------------------------------------
; Main evaluation (YOUR WORK GOES HERE)
;------------------------------------------------------------------------------

#|--------- BEGIN Helper functions imported from exercises ---------|#
(define (make-splitter splitter)
  (lambda (s) (if (sublist (string-split splitter) (string-split s))
                  (list
                   (takef (string-split s) (lambda (x) (not(equal? x (first (string-split splitter))))))
                   (rest(memf (lambda (x) (equal? x (last (string-split splitter)))) (string-split s)))
                   )
                  #f))
  )

(define (second-helper sub lst)
  (if (empty? sub)
      #t
      (if (or (empty? lst) (not(equal? (first sub) (first lst))))
          #f
          (second-helper (rest sub) (rest lst))
      )
  )
)

(define (sublist-helper sub lst counter)
  (if (empty? lst)
      #f
      (if (second-helper sub lst)
          counter
          (sublist-helper sub (rest lst) (+ counter 1))
      )
  )
)

(define (sublist sub lst)
  (if (empty? sublist)
      0
      (sublist-helper sub lst 0)
  )
)

#|--------- END Helper functions imported from exercises ---------|#

#| ------ BEGIN Helper functions derived from exercise functions ------ |#

(define add-splitter (make-splitter add))
(define mult-splitter (make-splitter mult))
(define func-splitter (make-splitter call))

#| ------ END Helper functions derived from exercise functions ------ |#

#|
Recursive function used to figure out what to do with each line. List contains a
list of each line of FunShake.
|#

#|(define (line-parser lst)
  (if (null? lst)
      void
      (cond
        [(equal? (first lst) personae) (display "***FOUND CHARACTERS***\n") (line-parser (rest lst))]
        [(equal? (first lst) settings) (display "***FOUND SETTINGS\n") (line-parser (rest lst))]
        [(equal? (first lst) finis) (display "***FOUND FINIS\n") (line-parser (rest lst))]
        [else (display "***REGULAR_TEXT***\n") (line-parser (rest lst))]
      )
      )
   )
|#
(define (line-parser lst vars)
  (if (null? lst)
      void
      (cond
        [(equal? (first lst) personae)  (personae-parser (rest lst vars))]
        [(equal? (first lst) finis)  (line-parser (rest lst vars))]
        [else  (line-parser (rest lst))]
      )
      )
   )
  



#|
Responsible for creating variables or "personae" in FunShake
|#
(define (personae-parser lst vars)
  (if (null? lst)
      void
      (cond
        [(equal? (first lst) finis)  (line-parser (rest lst))]
        [else  (makevar
                (evaluate-line
                 ""
                (substring (first lst) (string-length (first (string-split (first lst)))))
                vars)
                (substring (first (string-split (first lst)))
                          0
                          (- (string-length(first (string-split (first lst)))) 1) ) 
                vars
                )
              (personae-parser (rest lst) vars)]
      )
      )
   )

;Responsible for making a variable once the name and value are determined
(define (makevar val name vars)
  (append (list (lambda (x) (cond
                        [(equal? x "val") val]
                        [(equal? x "name") name]
                        [else void]
                        ))) vars))

#|
Responsible for creating functions or "settings" in FunShake
|#

(define (settings-parser lst)
  (void))

#|
Responsible for managing the "dialogue" of funshake (ie. the actual
computations). Has a recursive helper to get the value of each line.

lst   : list of valid funshake dialogue
vars  : data structure containing all the variables and functions
return: list of ints
|#

(define (dialogue-parser lst vars)
  (dialogue-parser-helper lst (list) vars))

(define (dialogue-parser-helper lst returnlist vars)
  (cond
    [(empty? lst) returnlist]
    [else (dialogue-parser-helper (rest(rest lst)) (append returnlist (list(evaluate-line (first lst) (second lst) vars))) vars)]
    )
  )

#|
Takes a line of funshake that needs to be evaluated and evaluates it. Responsible
for arithmetic and function calls as well as normal expressions.

name    : variable name of dialogue caller
dialogue: string of funshake dialogue
vars    : data structure containing all the variables and functions
returns : int
|#

(define (evaluate-line name dialogue vars)
  (let*
      ([addition (add-splitter dialogue)]
       [multiply (mult-splitter dialogue)]
       [funcall (func-splitter dialogue)])
    
    (cond
      #|Function calls|#
      [funcall (void)] ;INCOMPLETE
      #|Addition|#
      [addition (+ (evaluate-value name (first addition) vars) (evaluate-value name (last addition)) vars)]
      #|Multiplication|#
      [multiply (* (evaluate-value name (first multiply) vars) (evaluate-value name (last multiply)) vars)]
      #|All other expressions|#
      [else (evaluate-value name (string-split dialogue) vars)]
      )
    )
  )

#|
Takes in a line of funshake and determines its positive or negative value. Does
not perform arithmetic. Performs variable lookups.

name  : the name of the caller (variable name if you will)
str   : a string-split line of funshake (list format)
vars    : data structure containing all the variables and functions
return: int
|#

(define (evaluate-value name str vars)
  (let*([len (length str)])#|referred to as n in spec sheet|#
    (cond
      #| Variable name look up for direct name references |#
      ;[(and (= len 1) (member (first str) vartable)) (display (string-append "Var " (first str) " refs " name "\n")) 1]
      #| Variable name look up for self references |#
      ; Returns 1 for now. Doesnt actually look up the name
      [(and (= len 1) (member (first str) self-refs)) (display (string-append "Var " (first str) " selfrefs " name "\n")) 1]
      #| Simply count the number of characters in the string with respect to "bad-words"|#
      [else
       (let* ([bad-words (bad-word-counter str 0)]) #|referred to as b in specs|#
         (if (= bad-words 0)
             len
             (* -1 (expt 2 bad-words) len)
             )
         )
       ]
      )
   )
  )
#|
Takes in a line of funshake and determines the number of "bad words" in it.

str    : a string-split line of funshake (list format)
counter: number used to count number of occurences of bad words.
         ***SET TO 0 WHEN CALLING***
return : int
|#

(define (bad-word-counter str counter)
  (cond
    [(empty? str) counter]
    [else (if (member (first str) bad-words)
              (bad-word-counter (rest str) (+ counter 1))
              (bad-word-counter (rest str) counter)
              )]
    )
  )

#|
(evaluate body)
  body: a list of lines corresponding to the semantically meaningful text
  of a FunShake file.

  Returns a list of numbers produced when evaluating the FunShake file.
  This should be the main starting point of your work! Currently,
  it just outputs the semantically meaningful lines in the file.
|#

(define (evaluate body)

  ;(line-parser body)
  body
  )

;Find the variable or function with the given name in vars
(define (findvar name vars) (if (empty? vars) void (if (equal? ((first vars) "name") name) ((first vars) "val") (findvar name (rest vars)))))