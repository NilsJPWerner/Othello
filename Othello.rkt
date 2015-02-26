#lang typed/racket
(require typed/test-engine/racket-tests)
(require/typed 2htdp/image
   [#:opaque Image image?]
   [circle (-> Integer String String Image)]
   [square (-> Integer String String Image)]
   [rectangle (-> Number Number String String Image)]
   [text (-> String Number String Image)] 
   [beside (-> Image * Image)]
   [above (-> Image * Image)]
   [above/align (-> String Image * Image)]
   [overlay (-> Image * Image)])

(define-type Player (U 'black 'white))

(define-struct Pos
  ([row : Integer]  ;; an integer on the interval [0,7]
   [col : Integer]) ;; an integer on the interval [0,7]
  #:transparent)

(define-struct Board
  ([squares : (Listof (U Player 'none))]) ;; a list of length 64
  #:transparent)

(define-struct Game
  ([board : Board]
   [next  : Player])
  #:transparent)

(: new-game : Game) 
; Creates a new game with pieces in the middle and black starting
(define new-game
  (Game 
   (Board (list 'none 'none 'none 'none 'none 'none 'none 'none 
                'none 'none 'none 'none 'none 'none 'none 'none  
                'none 'none 'none 'none 'none 'none 'none 'none 
                'none 'none 'none 'white 'black 'none 'none 'none 
                'none 'none 'none 'black 'white 'none 'none 'none  
                'none 'none 'none 'none 'none 'none 'none 'none 
                'none 'none 'none 'none 'none 'none 'none 'none 
                'none 'none 'none 'none 'none 'none 'none 'none))  'black))

(: board-ref : Board Pos -> (U Player 'none))
; Returna the piece at the given square, or 'none.
(define (board-ref board pos)
  (list-ref (Board-squares board)
            (+ (* 8 (Pos-row pos)) (Pos-col pos))))

;-----------outflanks?----------

(: outflanks? : Board Player Pos -> Boolean)
; Returns true if that player can outflank the opponent 
; by placing a piece at that position.
(define (outflanks? xs p pos)
  (and (symbol=? (board-ref xs pos) 'none) (not (empty? (flips xs p pos)))))

;---------------flips-----------

(: flips : Board Player Pos -> (Listof Pos))
; Given a board, a player and a move position, returns the (possibly empty) 
; list of positions containing pieces to flip if that move is enacted.
(define (flips board player pos)
  (local {(define opposite (if (symbol=? player 'black) 'white 'black))
          (: pos+ : Pos Integer Integer -> Pos)
          ; Adds X to pos-row and Y to pos-col
          (define (pos+ pos X Y)
            (Pos (+ (Pos-row pos) X) (+ (Pos-col pos) Y)))
          (: list-trim : Integer Integer Pos (Listof Pos) -> (Listof Pos))
          ; Makes a list of pos going in (X,Y) direction
          (define (list-trim X Y posi list)
            (cond 
              [(or (< (Pos-row posi) 0) (< (Pos-col posi) 0)
                   (> (Pos-row posi) 7) (> (Pos-col posi) 7)) empty]
              [(symbol=? (board-ref board posi) 'none) empty]
              [(symbol=? (board-ref board posi) player) list]
              [else (list-trim X Y (pos+ posi X Y) (cons posi list))]))}
    (if (symbol=? (board-ref board pos) 'none) 
        (append
         (list-trim -1 0 (pos+ pos -1 0) '()) 
         (list-trim -1 -1 (pos+ pos -1 -1) '()) 
         (list-trim 0 -1 (pos+ pos 0 -1) '()) 
         (list-trim 1 -1 (pos+ pos 1 -1) '())
         (list-trim 1 0 (pos+ pos 1 0) '()) 
         (list-trim 1 1 (pos+ pos 1 1) '())
         (list-trim 0 1 (pos+ pos 0 1) '()) 
         (list-trim -1 1 (pos+ pos -1 1) '())) '())))

;-----------apply-move-----------

(: apply-move : Game Player Pos -> Game)
; Given a game, a player and a position, applies the move to 
; the game board and returns the subsequent game state.
(define (apply-move game player pos)
  (local {(: boardv : (Vectorof (U Player 'none)))
          (define boardv (list->vector (Board-squares (Game-board game))))
          (: replace : Pos -> Void)
          ; Replaces the given pos with player in boardv 
          (define (replace posi) 
               (vector-set! 
                    boardv (+ (* 8 (Pos-row posi)) (Pos-col posi)) player))}
    (cond
      [(not (symbol=? (Game-next game) player)) (error "wrong player")]
      [(not (outflanks? (Game-board game) player pos)) 
       (error "not a valid move")]
      [else (Game 
             (begin (map replace (append (list pos) 
                                         (flips (Game-board game) player pos))) 
                    (Board (vector->list boardv))) 
             (if (symbol=? player 'black) 'white 'black))])))
                                     
;----------game-over?-----------

(: game-over? : Game -> Boolean)
; Tests whether there are no more available outflanking moves (game over)
(define (game-over? game)
  (not (or (ormap (lambda ([x : Pos]) 
                    (outflanks? (Game-board game) 'white x)) pos-list)
           (ormap (lambda ([x : Pos]) 
                    (outflanks? (Game-board game) 'black x)) pos-list))))
        
(: pos-list : (Listof Pos))
; List of all pos on board
(define pos-list 
  (list (Pos 0 0) (Pos 0 1) (Pos 0 2) (Pos 0 3) (Pos 0 4) (Pos 0 5) 
        (Pos 0 6) (Pos 0 7) (Pos 1 0) (Pos 1 1) (Pos 1 2) (Pos 1 3) 
        (Pos 1 4) (Pos 1 5) (Pos 1 6) (Pos 1 7) (Pos 2 0) (Pos 2 1) 
        (Pos 2 2) (Pos 2 3) (Pos 2 4) (Pos 2 5) (Pos 2 6) (Pos 2 7) 
        (Pos 3 0) (Pos 3 1) (Pos 3 2) (Pos 3 3) (Pos 3 4) (Pos 3 5) 
        (Pos 3 6) (Pos 3 7) (Pos 4 0) (Pos 4 1) (Pos 4 2) (Pos 4 3) 
        (Pos 4 4) (Pos 4 5) (Pos 4 6) (Pos 4 7) (Pos 5 0) (Pos 5 1) 
        (Pos 5 2) (Pos 5 3) (Pos 5 4) (Pos 5 5) (Pos 5 6) (Pos 5 7)
        (Pos 6 0) (Pos 6 1) (Pos 6 2) (Pos 6 3) (Pos 6 4) (Pos 6 5) 
        (Pos 6 6) (Pos 6 7) (Pos 7 0) (Pos 7 1) (Pos 7 2) (Pos 7 3)
        (Pos 7 4) (Pos 7 5) (Pos 7 6) (Pos 7 7)))

;-----------outcome----------

(: outcome : Game -> (U Player 'tie))
; Tallies the pieces on the board and determines the outcome
(define (outcome game)
  (local {(define list (Board-squares (Game-board game)))
          (define black# (length (filter (lambda ([x : (U Player 'none)]) 
                                           (symbol=? x 'black)) list)))
          (define white# (length (filter (lambda ([x : (U Player 'none)]) 
                                           (symbol=? x 'white)) list)))}
    (cond
      [(> black# white#) 'black]
      [(< black# white#) 'white]
      [else 'tie])))

;---------board-image----------

(: board-image : Board Integer -> Image)
; Given a board and the desired width of the whole board image, 
; produces an image of the board.
(define (board-image board n)
  (local {(: side-nos : Integer (Image Image -> Image) -> Image)
          ; Makes a row or column of squares with numbers for sides of board
          (define (side-nos x align)
            (cond 
              [(= x 7) (box-number x n)]
              [else (align (box-number x n) (side-nos (+ x 1) align))]))
          (: board-row : (Listof (U Player 'none)) Integer -> Image)
          ; Makes a row of squares on the board
          (define (board-row list x)
            (cond
              [(= x 7) (box-color (first list) n)]
              [else (beside (box-color (first list) n)
                           (board-row (rest list) (+ x 1)))]))
          (: main-board : (Listof (U Player 'none)) Integer -> Image)
          ; Puts together all board-rows to make Board
          (define (main-board list x)
            (cond 
              [(= x 7) (board-row list 0)]
              [else (above (board-row list 0)
                            (main-board (list-tail list 8) (+ x 1)))]))}
    (above/align "left" 
                 (beside (box-color 'none n) (side-nos 0 beside))
                 (beside (side-nos 0 above) 
                         (main-board (Board-squares board) 0)))))
  
(: box-color : (U Player 'none) Integer -> Image)
;Takes a color and size of board and creates a board square with chosen color
(define (box-color color scale)
  (if (symbol=? color 'none)
      (square (quotient scale 9) "outline" "black")
      (overlay (square (quotient scale 9) "outline" "black") 
               (if (symbol=? color 'white)
                   (circle (quotient scale 24) "outline" "black") 
                   (circle (quotient scale 24) "solid" "black"))))) 

(: box-number : Integer Integer -> Image)
;Takes a number and size of board and creates a board square with chosen number
(define (box-number num scale)
  (overlay (square (quotient scale 9) "outline" "black")
           (text (number->string num) (quotient scale 15) "black")))

;-------------game-image--------------

(: game-image : Game Integer -> Image)
; Given a game and the desired width of the whole game image, produce an image
; of the game, including the board, an indication of which player is to move 
; next, and each player's current score.
(define (game-image game scale)
  (local {(define list (Board-squares (Game-board game)))
          (define black# (length (filter (lambda ([x : (U Player 'none)]) 
                                           (symbol=? x 'black)) list)))
          (define white# (length (filter (lambda ([x : (U Player 'none)]) 
                                           (symbol=? x 'white)) list)))}
    (above/align "left"
     (board-image (Game-board game) scale)
     (rectangle 1 (quotient scale 15) "solid" "white")
     (text (string-append "Black: " (number->string black#) 
                          "   " "White: " (number->string white#) 
                          "   " "Turn: " (symbol->string (Game-next game))) 
           (quotient scale 14) "black"))))

;; pairs
(define-struct (A B) Pair ([a : A] [b : B]) #:transparent)


(define-type Strategy (Game -> Pos))
;A Strategy may assume at least one 
;move is available, and may signal an error when no move is available.

(: first-move Strategy)
; Chooses the first available legal move, starting from the upper left 
; corner, and moving across each row left-to-right, a row at a time
(define (first-move game)
  (local {(: first-move-within : (Listof Pos) -> Pos)
          (define (first-move-within list)
            (match list
              ['() (error "no legal moves available")]
              [(cons hd tl) (if (outflanks? (Game-board game) 
                                            (Game-next game) hd)
                                hd (first-move-within tl))]))}
    (first-move-within pos-list)))

;---------------------------------------

(: human : Strategy)
; Evaluates read-line input and applies pos move. If input was not applicable 
; return error message and restart strategy
(define (human game)
  (local {(define input (read-line))
          (define output (if (string? input) (parse-pos input)
                             (begin (displayln "not valid input")
                                    (human game))))}
    (if (string? output) (begin (displayln "not valid input") (human game))
        output)))
      
(: parse-pos : String -> (U Pos String))
; Parses the string into a position on the board, or, if that is not possible, 
; returns the unparseable string as is
(define (parse-pos input)
  (local {(define list (map char->integer (string->list input)))}
    (if (and (= (length list) 2) 
             (andmap (lambda ([x : Integer]) 
                       (and (<= (- x 48) 7) (>= (- x 48) 0))) list))
        (Pos (- (first list) 48) (- (second list) 48)) input)))
      
;-----------------------------------------

(define-struct History
  ([moves : (Listof (Pair Player Pos))])
  #:transparent
  #:mutable)

(: game-history : History)
(define game-history (History empty))

(: play-loop : Game Strategy Strategy -> (Pair (Listof (Pair Player Pos)) Game))
; Administers the game between two strategys. Displays game and applies 
; strategy for each recursive turn. Ends game with a return of game state
; and history if game is over or too many invalid moves were chosen
(define (play-loop game strat1 strat2)
  (begin (displayln (game-image game 200))
         (local {(define move (apply-strat game strat1 0))
                 (define history (reverse (History-moves game-history)))}
           (if (or (game-over? game) (symbol? move)) 
               (begin (set-History-moves! game-history empty)
                      (Pair history game))
               (play-loop move strat2 strat1)))))
                                                 

(: apply-strat : Game Strategy Integer -> (U Game 'repeatfail))
; Applies strategy to game and updates history. If invalid move is chosen 3 
; time return failure indication. If player can't move, cycle to next player.
(define (apply-strat game strat tries)
  (local {(define player (Game-next game))
          (define opposite (if (symbol=? player 'black) 'white 'black))}
    (cond
      [(= tries 3) 'repeatfail]
      [(not (ormap (lambda ([x : Pos]) 
                     (outflanks? (Game-board game) player x)) pos-list))
       (Game (Game-board game) opposite)]
      [else (begin (define pos (strat game))
                   (if (outflanks? (Game-board game) player pos) 
                       (begin (set-History-moves! 
                               game-history 
                               (cons (Pair player pos) 
                                     (History-moves game-history))) 
                              (apply-move game player pos))
                       (begin (displayln "move not valid")
                              (apply-strat game strat (+ 1 tries)))))])))                                                                       

(: pass-and-play : -> (Pair (Listof (Pair Player Pos)) Game))
; Play loop with two human players
(define (pass-and-play)
  (play-loop new-game human human))

;----------------------------------

(: corners : (Listof Pos))
(define corners 
  (list (Pos 0 0) (Pos 0 7) (Pos 7 0) (Pos 7 7)))

(: edges : (Listof Pos))
(define edges 
  (list (Pos 0 1) (Pos 0 2) (Pos 0 3) (Pos 0 4) (Pos 0 5) (Pos 0 6)
        (Pos 1 0) (Pos 1 7) (Pos 2 0) (Pos 2 7) (Pos 3 0) (Pos 3 7)
        (Pos 4 0) (Pos 4 7) (Pos 5 0) (Pos 5 7) (Pos 6 0) (Pos 6 7)
        (Pos 7 1) (Pos 7 2) (Pos 7 3) (Pos 7 4) (Pos 7 5) (Pos 7 6)))

(: center : (Listof Pos))
(define center
  (list (Pos 1 1) (Pos 1 2) (Pos 1 3) (Pos 1 4) (Pos 1 5) (Pos 1 6)
        (Pos 2 1) (Pos 2 2) (Pos 2 3) (Pos 2 4) (Pos 2 5) (Pos 2 6) 
        (Pos 3 1) (Pos 3 2) (Pos 3 3) (Pos 3 4) (Pos 3 5) (Pos 3 6)
        (Pos 4 1) (Pos 4 2) (Pos 4 3) (Pos 4 4) (Pos 4 5) (Pos 4 6)
        (Pos 5 1) (Pos 5 2) (Pos 5 3) (Pos 5 4) (Pos 5 5) (Pos 5 6)
        (Pos 6 1) (Pos 6 2) (Pos 6 3) (Pos 6 4) (Pos 6 5) (Pos 6 6)))

(: immediate-tactics : Strategy)
; Picks corner moves first, if none are available pick edges, if none
; available pick first available move
(define (immediate-tactics game)
  (local {(define best 
            (findf (lambda ([x : Pos]) 
                     (outflanks? (Game-board game) (Game-next game) x)) 
                   (append corners edges pos-list)))}
    (if (boolean? best) (first-move game) best)))
          
;---------------------------------------

(define-type Heuristic (Game -> Integer))

(: counts-pieces : Player (Listof (U Player 'none)) -> Integer)
; Counts the number of pieces in a list according to chose player
(define (counts-pieces p list)
  (count (lambda ([x : (U Player 'none)]) (symbol=? x p)) list))

(: piece-counting : Heuristic)
; Calculates the number of black pieces minus number of white pieces
(define (piece-counting g)
  (- (counts-pieces 'black (Board-squares (Game-board g)))
     (counts-pieces 'white (Board-squares (Game-board g)))))

;........................................

(: prefer-edges : Integer -> Heuristic)
; Calculates the number of black pieces minus number of white pieces, weighting
; edge squares to a chosen value
(define (prefer-edges weight)
  (lambda ([game : Game])
    (local {(define game-center 
              (map (lambda ([x : Pos]) (board-ref (Game-board game) x))
                   center))
            (define game-edges 
              (map (lambda ([x : Pos]) (board-ref (Game-board game) x)) 
                   (append edges corners)))}                                    
      (- (+ (* weight (counts-pieces 'black game-edges))
            (counts-pieces 'black game-center))
         (+ (* weight (counts-pieces 'white game-edges))
            (counts-pieces 'white game-center))))))

;...........................................

(: prefer-edges-and-corners : Integer Integer -> Heuristic)
; Calculates the number of black pieces minus number of white pieces, weighting
; edge and corner squares to chosen values
(define (prefer-edges-and-corners edge-w corner-w)
  (lambda ([game : Game])
    (local 
      {(define game-center 
         (map (lambda ([x : Pos]) (board-ref (Game-board game) x)) center))
       (define game-corners 
         (map (lambda ([x : Pos]) (board-ref (Game-board game) x)) corners))
       (define game-edges 
         (map (lambda ([x : Pos]) (board-ref (Game-board game) x)) edges))}
      (- (+ (* corner-w (counts-pieces 'black game-corners))
            (* edge-w (counts-pieces 'black game-edges))
            (counts-pieces 'black game-center))
         (+ (* corner-w (counts-pieces 'white game-corners))
            (* edge-w (counts-pieces 'white game-edges))
            (counts-pieces 'white game-center))))))

;------------------------------------------
(: minimax-eval : Heuristic Integer Game -> Integer)
; Consumes the ply and a game, and assigns a score using 
; the given heuristic function.
(define (minimax-eval heuristic ply game)
  (local {(define scores 
            (cond 
              [(game-over? game) (list (piece-counting game))] 
              [(= ply 0) (list (heuristic game))]
              [else (map (lambda ([x : Pos])   
                           (minimax-eval heuristic (- ply 1) 
                                         (apply-move game (Game-next game) x)))
                         (filter (lambda ([x : Pos]) 
                                   (outflanks? (Game-board game) 
                                               (Game-next game) x)) 
                                 pos-list))]))}
    (if (empty? scores)
        (minimax-eval heuristic ply 
                      (Game (Game-board game) 
                            (if (symbol=? (Game-next game) 
                                          'black) 'white 'black)))
        (if (symbol=? 'black (Game-next game))
            (apply max scores) (apply min scores)))))

;.......................................

(: minimax : Heuristic Integer -> Strategy)
; Applies minimax algorithm given a Heuristic and Integer to create
; a Strategy
(define (minimax heuristic ply)
  (lambda ([game : Game])
    (Pair-a 
     (first
      (sort (map 
             (lambda ([x : Pos]) 
               (Pair x (minimax-eval heuristic (- ply 1)
                                     (apply-move game (Game-next game) x))))
             (filter (lambda ([x : Pos])                             
                       (outflanks? (Game-board game) 
                                   (Game-next game) x)) pos-list))
            (lambda ([x : Pair] [y : Pair]) 
              ((if (symbol=? (Game-next game) 'black) > <) 
               (Pair-b x) (Pair-b y))))))))

;---------------------------------------------

(: pick-upto : (All (a) Integer (Listof a) -> (Listof a)))
; Choses a chosen number of items randomly from a list
(define (pick-upto num list)
  (if (>= num (length list)) list
      (local {(: pick-acc : Integer (Listof a) (Listof a) -> (Listof a))
              (define (pick-acc no orig dest)
                (local {(define rand (random (length orig)))
                        (define pick (list-ref orig rand))}
                  (if (= no 0) dest 
                      (pick-acc (sub1 no) (remove pick orig) 
                                (cons pick dest)))))}
        (pick-acc num list '()))))


(: montymax : Heuristic Integer Integer -> Strategy)
; Applies minimax algorithm but with the branches on each level limited
; by a branch limiting factor
(define (montymax heuristic ply pick)
  (lambda ([game : Game])
    (Pair-a 
     (first
      (sort (map 
             (lambda ([x : Pos]) 
               (Pair x (montymax-eval 
                        heuristic (- ply 1)
                        (apply-move game (Game-next game) x) pick)))
             (pick-upto
              pick
              (filter (lambda ([x : Pos])                             
                        (outflanks? (Game-board game) 
                                    (Game-next game) x)) pos-list)))
            (lambda ([x : Pair] [y : Pair]) 
              ((if (symbol=? (Game-next game) 'black) > <) 
               (Pair-b x) (Pair-b y))))))))


(: montymax-eval : Heuristic Integer Game Integer -> Integer)
; Consumes the ply, game and a max branch factor to constrain searches, 
; and assigns a score using the given heuristic function.
(define (montymax-eval heuristic ply game pick)
  (local {(define scores 
            (if (or (game-over? game) (= ply 0)) (list (heuristic game))
                (map (lambda ([x : Pos])   
                       (montymax-eval 
                        heuristic (- ply 1) 
                        (apply-move game (Game-next game) x) pick))
                     (pick-upto 
                      pick (filter (lambda ([x : Pos]) 
                                     (outflanks? (Game-board game) 
                                                 (Game-next game) x)) 
                                   pos-list)))))}
    (if (empty? scores)
        (montymax-eval heuristic ply 
                       (Game (Game-board game) 
                             (if (symbol=? (Game-next game) 
                                           'black) 'white 'black)) pick)
        (if (symbol=? 'black (Game-next game))
            (apply max scores) (apply min scores)))))

;----------------- TESTS -------------------

(check-expect (first-move new-game) (Pos 2 3))
(check-expect (first-move game2) (Pos 1 3))
(check-expect (first-move game3) (Pos 0 1))
(check-expect (first-move game4) (Pos 0 2))
(check-error (first-move endgame) "no legal moves available")

(check-expect (parse-pos "11") (Pos 1 1))
(check-expect (parse-pos "99") "99")
(check-expect (parse-pos "04") (Pos 0 4))
(check-expect (parse-pos "lil") "lil")

(check-expect 
 (apply-strat new-game first-move 0)
 (Game 
  (Board (list 'none 'none 'none 'none 'none 'none 'none 'none 
               'none 'none 'none 'none 'none 'none 'none 'none  
               'none 'none 'none 'black 'none 'none 'none 'none 
               'none 'none 'none 'black 'black 'none 'none 'none 
               'none 'none 'none 'black 'white 'none 'none 'none  
               'none 'none 'none 'none 'none 'none 'none 'none 
               'none 'none 'none 'none 'none 'none 'none 'none 
               'none 'none 'none 'none 'none 'none 'none 'none))  
  'white))
(check-expect 
 (apply-strat game2 immediate-tactics 0) 
 (Game 
  (Board (list 'white 'none 'none 'none 'none 'none 'none 'none 
               'none 'none 'white 'white 'none 'none 'none 'none  
               'none 'none 'none 'white 'none 'white 'none 'none 
               'white 'none 'none 'white 'black 'black 'none 'none 
               'white 'black 'white 'white 'black 'none 'none 'none  
               'none 'none 'white 'none 'none 'none 'black 'none 
               'none 'none 'white 'none 'none 'none 'none 'none 
               'none 'none 'white 'none 'none 'none 'none 'none))  
  'black))
(check-expect 
 (apply-strat game3 (minimax piece-counting 2) 0)
 (Game 
  (Board (list 'none 'black 'none 'none 'none 'white 'none 'none 
               'none 'none 'black 'white 'none 'white 'none 'none  
               'white 'none 'none 'black 'none 'white 'none 'none 
               'white 'none 'none 'black 'black 'black 'none 'none 
               'white 'black 'white 'white 'black 'none 'none 'none  
               'black 'black 'white 'none 'black 'black 'black 'none 
               'none 'none 'white 'none 'black 'none 'black 'none 
               'white 'none 'none 'none 'none 'none 'none 'none))  
  'white))
(check-expect 
 (apply-strat endgame first-move 0)
 (Game 
   (Board (list 'white 'black 'black 'black 'black 'black 'black 'black 
                'white 'black 'white 'black 'white 'black 'black 'white  
                'white 'white 'black 'black 'white 'white 'black 'white 
                'white 'black 'black 'black 'black 'black 'white 'white 
                'white 'black 'white 'white 'black 'black 'black 'white  
                'white 'white 'white 'white 'black 'white 'black 'white 
                'white 'black 'white 'black 'black 'black 'white 'white 
                'black 'black 'black 'black 'white 'white 'black 'white))  
   'black))
(check-expect 
 (apply-strat new-game first-move 3) 'repeatfail)            

(check-expect (immediate-tactics new-game) (Pos 2 3))
(check-expect (immediate-tactics game2) (Pos 1 3))
(check-expect (immediate-tactics game3) (Pos 0 1))
(check-expect (immediate-tactics game4) (Pos 0 2))
(check-error (immediate-tactics endgame) "no legal moves available")

(check-expect (counts-pieces 'black (Board-squares (Game-board new-game))) 2)
(check-expect (counts-pieces 'white (Board-squares (Game-board game2))) 10)
(check-expect (counts-pieces 'white (Board-squares (Game-board game3))) 14)
(check-expect (counts-pieces 'black (Board-squares (Game-board game4))) 17)
(check-expect (counts-pieces 'white (Board-squares (Game-board endgame))) 30)

(check-expect (piece-counting new-game) 0)
(check-expect (piece-counting game2) -3)
(check-expect (piece-counting game3) -2)
(check-expect (piece-counting game4) 1)
(check-expect (piece-counting endgame) 4)

(check-expect ((prefer-edges 2) new-game) 0)
(check-expect ((prefer-edges 2) game2) -7)
(check-expect ((prefer-edges 2) game3) -6)
(check-expect ((prefer-edges 3) game4) -5)
(check-expect ((prefer-edges 4) endgame) -8)

(check-expect ((prefer-edges-and-corners 2 4) new-game) 0)
(check-expect ((prefer-edges-and-corners 2 3) game2) -8)
(check-expect ((prefer-edges-and-corners 2 4) game3) -8)
(check-expect ((prefer-edges-and-corners 3 5) game4) -3)
(check-expect ((prefer-edges-and-corners 4 6) endgame) -8)

(check-expect (minimax-eval piece-counting 2 new-game) 0)
(check-expect (minimax-eval (prefer-edges 2) 2 game2) -6)
(check-expect (minimax-eval (prefer-edges-and-corners 4 6) 3 game3) 14)
(check-expect (minimax-eval (prefer-edges 3) 2 game4) -7)
(check-expect (minimax-eval (prefer-edges-and-corners 4 6) 2 game4) -8)
(check-expect (minimax-eval (prefer-edges-and-corners 2 3) 2 endgame) 4)

(check-expect ((minimax piece-counting 2) new-game) (Pos 2 3))
(check-expect ((minimax piece-counting 2) game2) (Pos 1 3))
(check-expect ((minimax piece-counting 3) game3) (Pos 1 0))
(check-expect ((minimax piece-counting 2) game4) (Pos 0 3))
(check-expect ((minimax (prefer-edges-and-corners 2 3) 2) game2) (Pos 1 3))
(check-expect ((minimax (prefer-edges-and-corners 2 4) 3) game3) (Pos 1 0))
(check-expect ((minimax (prefer-edges-and-corners 2 4) 2) game4) (Pos 7 4))

(check-expect (length (pick-upto 3 (list 1 2 3 4 5))) 3)
(check-expect (length (pick-upto 4 (list "a" "b" "c" "d" "e"))) 4)
(check-expect (length (pick-upto 5 (list 3 4 5))) 3)
(check-expect (length (pick-upto 2 (list 1 2 3 4 5))) 2)

(check-expect (Pos? ((montymax piece-counting 2 4) new-game)) #t)
(check-expect (Pos? ((montymax piece-counting 3 2) game2)) #t)
(check-expect (Pos? ((montymax piece-counting 2 4) game3)) #t)
(check-expect (Pos? ((montymax piece-counting 3 2) game4)) #t)


;--------GAMES------------


(: game2 : Game) 
(define game2
  (Game 
   (Board (list 'white 'none 'none 'none 'none 'none 'none 'none 
                'none 'none 'white 'none 'none 'none 'none 'none  
                'none 'none 'none 'black 'none 'white 'none 'none 
                'white 'none 'none 'black 'black 'black 'none 'none 
                'white 'black 'white 'white 'black 'none 'none 'none  
                'none 'none 'white 'none 'none 'none 'black 'none 
                'none 'none 'white 'none 'none 'none 'none 'none 
                'none 'none 'white 'none 'none 'none 'none 'none))  'white))

(: game3 : Game) 
(define game3
  (Game 
   (Board (list 'none 'none 'none 'none 'none 'white 'none 'none 
                'none 'none 'white 'white 'none 'white 'none 'none  
                'white 'none 'none 'white 'none 'white 'none 'none 
                'white 'none 'none 'black 'black 'black 'none 'none 
                'white 'black 'white 'white 'black 'none 'none 'none  
                'black 'black 'white 'none 'black 'black 'black 'none 
                'none 'none 'white 'none 'black 'none 'black 'none 
                'white 'none 'none 'none 'none 'none 'none 'none))  'black))

(: game4 : Game) 
(define game4
  (Game 
   (Board (list 'black 'black 'none 'none 'none 'white 'none 'none 
                'none 'black 'white 'white 'none 'black 'none 'none  
                'white 'white 'none 'white 'black 'white 'none 'none 
                'white 'none 'none 'black 'white 'black 'none 'none 
                'white 'black 'white 'white 'black 'none 'none 'none  
                'none 'black 'white 'black 'black 'black 'black 'none 
                'none 'black 'white 'white 'black 'none 'black 'none 
                'none 'none 'white 'none 'none 'none 'none 'none))  'black))

(: endgame : Game) 
(define endgame
  (Game 
   (Board (list 'white 'black 'black 'black 'black 'black 'black 'black 
                'white 'black 'white 'black 'white 'black 'black 'white  
                'white 'white 'black 'black 'white 'white 'black 'white 
                'white 'black 'black 'black 'black 'black 'white 'white 
                'white 'black 'white 'white 'black 'black 'black 'white  
                'white 'white 'white 'white 'black 'white 'black 'white 
                'white 'black 'white 'black 'black 'black 'white 'white 
                'black 'black 'black 'black 'white 'white 'black 'white))  
   'white))

(test)

