;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; POMOCNE PROMENJLJIVE I POMOCNE FUNKCIJE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar letters '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
(defvar numbers '(0 1 2 3 4 5 6 7 8 9))
(defvar lettermap '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) 
(9 9) (A 10) (B 11) (C 12) (D 13) (E 14) (F 15) (G 16) (H 17) (I 18)
(J 19) (K 20) (L 21) (M 22) (N 23) (O 24) (P 25) (Q 26) (R 27)
(S 28) (T 29) (U 30) (V 31) (W 32) (X 33) (Y 34) (Z 35)))

(setq counter1 0)
(setq counter2 0)
(setq counter3 0)
(setq counter4 -1)
(setq endCounter 0)

;brojaci za liste i ostalo ^^^^

(setq counterTopRow 0)
(setq counterTopRowSkip 0)
(setq nextTopElement 0)
(setq counterBottomRow 0)
(setq counterBottomRowSkip 0)
(setq nextBottomElement 0)
(setq counterTopRowCell 0)
(setq counterRow 0)
(setq counterBottomRowCell 0)
(setq bottomCounterHelper 0)
(setq counterMiddleRow 0)
(setq nextMiddleElement 0)

;brojaci za iscrtavanje ^^^^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; POMOCNE PROMENJLJIVE I POMOCNE FUNKCIJE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRAVLJENJE LISTI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generateFirstDim (size curr)      
(cond                          
        ((= curr 0) ())       
        (T  (generateFirstDim size (- curr 1))) 
    )  
)                                               

(defun generateSecondDim (currentSize size) 
    (prog1 (cons counter2 (list (generateFirstDim size size ))) (setq counter2 (1+ counter2)))
)

(defun generateThirdDim (size currentSize)    
    (cond 
        ((= currentSize 0) ())
        (T
            (cons (generateSecondDim currentSize size) (generateThirdDim size (- currentSize 1))) 
        )
    )                                                          
)     

(defun generateState (size)
    (generateThirdDim size size2)
)

(defun printState (state)
    (print state)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRAVLJENJE LISTI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DODAVANJE POTEZA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun playAt ()
        (let* ((insert (read )) (column (car (cdr (assoc insert lettermap)))))
            (playMove column)
        )                     
)

(defun playMove (index)
    (if
        (< (length (car (cdr (assoc index state)))) size) 
        (progn
            (if (string-equal alternator t) (playMoveX index) (playMoveO index)) (setq alternator (not alternator)) (setq endCounter (1+ endCounter))
        )
        (write-line "Selected column already full, please pick another column")
    )
    (if 
        (= endCounter size3) (endGame)
    )
    (printBoard size)
)

(defun playMoveX (index)
    (setq state (addPlay 'X index state 0))            
)

(defun playMoveO (index)
    (setq state (addPlay 'O index state 0))   
)

(defun addPlay (play index state cnt)
    (cond
        ((< cnt index ) (cons (car state) (addPlay play index (cdr state) (1+ cnt))))                                                                                                   
        (T (cons (list (car (car state)) (append (nth 1 (car state)) (list play))) (cdr state)))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DODAVANJE POTEZA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENDGAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun endGame()
    (pointsChecker 'X)
    (pointsChecker 'O)
    (printBoard size)
    (fresh-line)
    (princ "The game has ended.")
    (fresh-line)
    (progn (princ "Points X: ") (princ pointsX))
    (fresh-line)
    (progn (princ "Points O: ") (princ pointsO))
    (fresh-line)
    (cond 
        ((= pointsX pointsO) (princ "It's a draw."))
        ((> pointsX pointsO) (princ "Player X is the winner!"))
        ((t (princ "Player O is the winner!")))
    )
    (quit)
)

(defun pointsChecker (player)
    (setq nn 0)
    (loop for posX from 0 to (1- size) do
        (loop for posY from 0 to (1- size) do
            (loop for posZ from 0 to (1- size) do               
                (loop for x from -1 to 1 do
                    (loop for y from -1 to 1 do
                        (loop for z from -1 to 1 do
                            (if (not (and (= x 0) (= y x) (= z x)))                   
                                (progn 
                                    (setq n 0)
                                    (loop for j from 0 to (- size 1) do
                                        (setq checkX (+ posX (* j x)))
                                        (setq checkY (+ posY (* j y)))
                                        (setq checkZ (+ posZ (* j z)))
                                        (if 
                                            (not (or (> checkX (1- size))(< checkX 0)(> checkY (1- size))(< checkY 0)(> checkZ (1- size)) (< checkZ 0))) 
                                            (if
                                                (string-equal (nth checkZ (car (cdr (assoc (+ checkX (* checkY size)) state)))) player)
                                                (setq n (1+ n))                                                   
                                                (return )
                                            )
                                        )
                                    )                                   
                                    (if 
                                        (>= n 4)      
                                        (setq nn (1+ nn))                                                                                                                                    
                                    ) 
                                )
                            )
                        )
                    )
                )
            )
        )
    )

    (if 
        (string-equal player 'X) 
        (setq pointsX (ceiling nn 2))
        (setq pointsO (ceiling nn 2))
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENDGAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CRTANJE TABLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun printBoard (size)
    (writeOrientationRow size)
    (writeRestRow size 0)
    (fresh-line)
    (writeOrientationRow size)
)

(defun writeRestRow (size currentSize) 
    (setq counterTopRowCell (1- size))
    (setq counterBottomRowCell (1- size))
    (setq counterRow 0)
    (setq bottomCounterHelper (- size 2))
    (loop while (not (= counterRow (- (* size 2) 1))) do          
        (cond
            ((< counterRow (- size 1)) (prog1 (writeTopRows size counterTopRowCell 0 counterRow) (setq counterTopRowCell (1- counterTopRowCell)) (fresh-line)))
            ((> counterRow (- size 1)) (prog1 (writeBottomRows size counterBottomRowCell 0 counterRow bottomCounterHelper)(setq bottomCounterHelper (1- bottomCounterHelper)) (setq counterBottomRowCell (1- counterBottomRowCell)) (fresh-line)))
            (T (writeMiddleRow size counterBottomRowCell 0 counterRow))
        )
    (setq counterRow (1+ counterRow))) 
)

(defun writeMiddleRow (size counterCell counterEnd rowNumber)
    (setq counterMiddleRow 0)
    (setq nextMiddleElement (1- size))
    (loop while (not (= counterMiddleRow size2)) do 
        (getElement nextMiddleElement counterMiddleRow)
        (if (= nextMiddleElement 0) (setq nextMiddleElement (1- size)) (setq nextMiddleElement (1- nextMiddleElement)))  
        (setq counterMiddleRow (1+ counterMiddleRow))
    )
    (fresh-line)
)

(defun writeTopRows (size counterCell counterEnd rowNumber)        
    (setq counterTopRowSkip 0) 
    (setq nextTopElement (1- size))
    (loop while (not (= counterTopRow size2)) do         
    (cond 
        ((< counterTopRowSkip counterCell) (progn (write-string "  ") (setq counterTopRowSkip (1+ counterTopRowSkip))))
        ((= counterTopRowSkip (1- size)) (progn (setq counterTopRowSkip 0)  (getElement nextTopElement counterTopRow) (setq nextTopElement (1- size))))
        (T (progn (setq counterTopRowSkip (1+ counterTopRowSkip)) (getElement nextTopElement counterTopRow) (setq nextTopElement (1- nextTopElement))        ))
    )
    (setq counterTopRow (1+ counterTopRow))
    )     
    (setq counterTopRow 0)
)

(defun getElement (nextEl colNumber)
    (if (nth nextEl (car (cdr (assoc colNumber state)))) (progn (write (nth nextEl (car (cdr (assoc colNumber state))))) (write-string " ")) ( write-string "- "))
)

(defun writeBottomRows (size counterCell counterEnd rowNumber nextBottomEl)    
    (setq counterBottomRowSkip 0)
    (setq nextBottomElement nextBottomEl)
    (setq helperCounter 1)

    (loop while (not (= counterBottomRow size2)) do
        (cond      
            ((< counterBottomRowSkip counterCell) (progn (setq counterBottomRowSkip (1+ counterBottomRowSkip)) (getElement nextBottomElement counterBottomRow) (setq nextBottomElement (1- nextBottomElement))))
            ((= counterBottomRowSkip (1- size)) (progn (setq counterBottomRowSkip 0) (setq nextBottomElement nextBottomEl) (write-string "  ")))     
            (T (progn (write-string "  ") (setq counterBottomRowSkip (1+ counterBottomRowSkip))))
        )
        (setq counterBottomRow (1+ counterBottomRow))
    )
    (setq counterBottomRow 0)
)

(defun writeOrientationRow (size)
    (loop while (not (= counter4 (1- size2))) do
        (setq counter4 (1+ counter4))
        (if 
            (< counter4 (length numbers)) (princ (nth counter4 numbers))
        )
        (if 
            (>= counter4 (length numbers)) (progn (princ (nth counter3 letters)) (setq counter3 (1+ counter3)))
        )
        (write-string " ")
    )
    (fresh-line)
    (setq counter4 -1)
    (setq counter3 0)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CRTANJE TABLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Welcome to Connect-Four-3D. Made as a project for the AI course at the Faculty of Electronic Engineering, Nis.")
(print "To start playing, follow the instructions below. Unlike the classic connect four game, this one plays out in a way that sums up how many fours have both players made (in 3D) and then declares the winner once the board has been completely filled.")
(write-line "")
(write-line "Enter field size (Valid if even number, 4 or 6 optimally):")

(setq size (read ))
(cond 
    ((and (= (mod size 2) 0) (> size 3))) (t (write-line "Invalid size, exiting...") (quit))
)
(setq size2 (* size size))
(setq size3 (* size2 size))
(setq state '((0 (X X O X X O)) (1 (X X X X X O)) (2 (X X X X X O)) (3 (X X X X X O)) (4 (X X X X X O)) (5 (X X X X X O)) (6 (X X O X X O)) (7 (X X X X X O)) (8 (O O O O X O)) 
(9 (O X O O X O)) (10 (O X O O X O)) (11 (O O O O X O)) (12 (O O O O X O)) (13 (O O O O X O)) (14 (O O O O X O)) (15 (O O O O X O)) (16 (X X O X X O)) (17 (X X O X X O)) (18 (X X O X X O)) (19 (X X O X X O)) 
(20 (X X O X X O)) (21 (X X O X X O)) (22 (X X O X X O)) (23 (X X O X X O)) (24 (X X O X X O)) (25 (X X O X X O)) (26 (X X O X X O)) (27 (X X O X X O)) 
(28 (X X O X X O)) (29 (X X O X X O)) (30 (X X O X X O)) (31 (X X O X X O))
(32 (X X O X X O)) (33 (X X O X X O)) (34 (X X O X X O)) (35 (X X O X O))))
(setq endCounter 215)
;(setq state (generateState size))


(print "Pick starting player, X or O: (if invalid player, default will be set to X")
(setq starter (read ))
(if (string-equal starter "O") (setq alternator NIL) (setq alternator T))

(print "For players: type play and then the corresponding column to insert a piece (e.g. play 2, play h); type exit to quit the game.")
(print "status: Shows the number of pieces inserted, and how many more must be inserted to end the game.")
(print "pointsX: Current points for player X.")
(print "pointsO: Current points for player O.")
(print "Technical commands:")
(print "layout: Shows the list layout.")
(fresh-line)
(format t " ")
(fresh-line)
(printBoard size)

(write-line "")
(setq action (read ))
(loop while (not (string-equal action "exit")) do
    (cond 
        ((string-equal action "layout") (printState state))
        ((string-equal action "node") (print node))
        ((string-equal action "showboard") (printBoard size))
        ((string-equal action "pointsX") (progn (pointsChecker 'X) (print pointsX)))
        ((string-equal action "pointsO") (progn (pointsChecker 'O) (print pointsO)))
        ((string-equal action "status") (progn (princ "Total pieces inserted: ") (princ endCounter) 
        (fresh-line) (princ "You must insert ") (princ (- size3 endCounter)) (princ " more pieces to end the game.")))
        ((string-equal action "play") (playAt))
        (T (write-line "Please pick a correct action."))
    )
    (write-line "")
    (write-line "Choose another action:")
    (write-line "")
    (setq action (read ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
