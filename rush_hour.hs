{-
  This code has been tested in GHCi, version 8.0.2
  Authos: Merkouris Papamichail, Antonia Tsili
-}

import Data.Map as Map
import Data.Set as Set

{-	***********
	***State***
	*********** -}



--Grid

--τύπος δεδομένων
{- 	Ορίζουμε ως πλέγμα ένα υποσύνολο του Ν x N. Συγκεκριμένα δεδομένου
	ενός άκρου grid_edge, θα έχουμε grid = {(i,j) | 1 <= i <= grid_edge.i, 1 <= j <= grid_edge.j}.
	Στη συνέχεια θα αναφερόμαστε στον τύπο (Int, Int) της haskell ως σημείο.-}
data Grid = Grid (Int, Int) deriving (Show, Eq) --Σύνολο σημείων άκρο

--πράξεις
--για σωστή κλήση της συνάρτησης: grid_edge grid
grid_edge :: Grid -> 	(Int, Int)
grid_edge 	(Grid grid_edge) = grid_edge

--για σωστή κλήση της συνάρτησης: grid_from_string cs (0,0)
grid_from_string ::	 String -> 	(Int, Int) -> 	Grid
grid_from_string 	('\n':[]) 	(i, j) 			= (Grid (i + 1,j))

grid_from_string (c:cs) (i, j)
	|(c == '\n') = grid_from_string cs (i + 1, 0)
	|otherwise	 = grid_from_string cs (i, j + 1)

--για σωστή κλήση της συνάρτησης: tranverse grid (i, j)
grid_traverse :: Grid -> 			(Int, Int) -> (Int, Int)
grid_traverse 	(Grid (e_i, e_j)) 	(i, j)
	|(i <= e_i && j + 1 <= e_j) = (i, j + 1)
	|(i + 1 <= e_i && j == e_j) = (i + 1, 1)
	|otherwise 					= (0,0)


grid_touches_the_edge ::	Grid ->	Bool ->			(Int, Int) ->	Bool
grid_touches_the_edge		grid	is_horizontal	endpoint
	|is_horizontal	= (snd edge) == (snd endpoint) || 1 == (snd endpoint)
	|otherwise		= (fst edge) == (fst endpoint) || 1 == (fst endpoint)
	where
		edge = grid_edge grid


grid_touches_the_right_edge ::	Grid ->	(Int, Int) ->	Bool
grid_touches_the_right_edge		grid	endpoint		= snd endpoint == snd (grid_edge grid)



--PointColoring

--τύπος δεδομένων
data PointColoring = PointColoring (Map (Int, Int) Char) deriving (Show, Eq)


--πράξεις
--για σωστή κλήση της συνάρτησης: pnt_clr_expand_coloring point_coloring (i,j) c
pnt_clr_expand :: 	PointColoring -> 	(Int, Int) -> 	Char -> 		PointColoring
pnt_clr_expand	 	(PointColoring m) 	point 			value			= (PointColoring (Map.insert point value m))


pnt_clr_get_clr :: PointColoring -> (Int, Int) -> Char
pnt_clr_get_clr (PointColoring m) (i, j) = m ! (i,j)


--για σωστή κλήση της συνάρτησης: pnt_clr_from_string cs (0,0) (PointColoring Map.Empty)
pnt_clr_from_string :: 	String -> 	(Int, Int) -> 	PointColoring -> 	PointColoring
pnt_clr_from_string 	(c:[]) 		(i,j) 			pnt_clr 			= pnt_clr

pnt_clr_from_string 	(c:cs) 		(i, j) 			pnt_clr
	|(c == '\n')	= pnt_clr_from_string cs (i + 1, 0) pnt_clr
	|otherwise 		= pnt_clr_from_string cs (i, j + 1) (pnt_clr_expand pnt_clr (i + 1, j + 1) c)


--για σωστή κλήση της συνάρτησης: pnt_clr_to_string p_c (1,1) grid []
pnt_clr_to_string :: 	PointColoring -> 	(Int, Int) -> 	Grid -> String -> String
pnt_clr_to_string		p_c					cursor			grid	cs
	|cursor == (grid_edge grid) 			= reverse (('\n' : (pnt_clr_get_clr p_c cursor) : cs))
	|(snd cursor) == (snd (grid_edge grid)) = pnt_clr_to_string p_c (grid_traverse grid cursor) grid ('\n': ((pnt_clr_get_clr p_c  cursor) : cs))
	|otherwise								= pnt_clr_to_string p_c (grid_traverse grid cursor) grid ((pnt_clr_get_clr p_c  cursor) : cs)


--InverseLineSegmentColoring

--τύπος δεδομένων
data InverseColoring = InverseColoring (Map Char (Int, Int)) deriving (Show, Eq)


--πράξεις
--για σωστή κλήση της συνάρτησης: inv_clr_get_inverse_coloring i_c replacment_func c (i,j)
inv_clr_expand :: 	InverseColoring -> 		((Int, Int) -> (Int, Int) -> (Int, Int)) -> 	Char -> (Int, Int) -> InverseColoring
inv_clr_expand		(InverseColoring i_c)	replacement_func								color	point

	= InverseColoring (Map.insertWith (replacement_func) color point i_c)


--για σωστή κλήση της συνάρτησης: inv_clr_get_inverse_coloring grid (1,1) p_c {(\new_v old_v -> new_v) | (\new_v old_v -> old_v)}(*) (InverseColoring Map.empty)
inv_clr_get_inverse_coloring :: Grid ->	(Int, Int) ->	PointColoring -> 	((Int, Int) -> (Int, Int) -> (Int, Int)) -> 	InverseColoring -> 	InverseColoring
inv_clr_get_inverse_coloring	grid	(0,0)			p_c					replacement_func								i_c		= i_c

inv_clr_get_inverse_coloring 	grid 	cursor 			p_c		 			replacement_func 								i_c
	
	= inv_clr_get_inverse_coloring 	grid new_cursor p_c replacement_func expanded_i_c
	where
		new_cursor = grid_traverse grid cursor
		expanded_i_c = inv_clr_expand i_c replacement_func (pnt_clr_get_clr p_c cursor) cursor

--(*)τυπικά μπορούμε οποιαδήποτε συνάρτηση ((Int, Int) -> (Int, Int) -> (Int, Int)), αλλά εμείς θα χρησιμοποιήσουμε μόνο αυτές τις δύο


--τύπος δεδομένων
data InverseLineSegmentColoring = InverseLineSegmentColoring (Map Char ((Int, Int), (Int, Int))) deriving (Show, Eq) --χρώμα αρχή τέλος --State 
																		--αρχή		τέλος

--πράξεις
--για σωστή κλήση της συνάρτησης: ilsc_get_inverse_coloring ic_s [\new_v old_v -> old_v] ic_e [\new_v old_v -> old_v] (InverseLineSegmentColoring Map.empty)
ilsc_get_inverse_coloring :: InverseColoring -> 	InverseColoring -> 		InverseLineSegmentColoring -> 		InverseLineSegmentColoring
ilsc_get_inverse_coloring (InverseColoring ic_s) 	(InverseColoring ic_e) 	(InverseLineSegmentColoring ilsc)
	|(ic_s == Map.empty && ic_e == Map.empty) 	= (InverseLineSegmentColoring (Map.delete '.' ilsc)) --(*)
	|otherwise									= ilsc_get_inverse_coloring (InverseColoring (Map.deleteMin ic_s)) (InverseColoring (Map.deleteMin ic_e))
													(InverseLineSegmentColoring (Map.insert (fst (Map.findMin ic_s)) (snd (Map.findMin ic_s), snd (Map.findMin ic_e))  ilsc))

--(*)δεν θέλουμε το ευθύγραμμο τμήμα με το χρώμα '.', γιατί φυσικά δεν είναι ευθύγραμμο τμήμα, οπότε οι υπολογισμοί που έχουν γίνει γι' αυτό δεν έχουν νόημα
--   εξ άλλου δεν συμπεριφέρεται όπως τα ευθύγραμμα τμήματα (αυτοκίνητα) μέσα στο παιχνίδι, δεν μπορούμε να μετακινούμε τον κενό χώρο.


ilsc_get_endpoints :: 	InverseLineSegmentColoring -> 		Char ->	((Int, Int), (Int, Int))
ilsc_get_endpoints		(InverseLineSegmentColoring ilsc)	color		= ilsc ! color


ilsc_get_reds_endpoints :: 	InverseLineSegmentColoring ->	((Int, Int), (Int, Int))
ilsc_get_reds_endpoints		ilsc							= ilsc_get_endpoints ilsc '='


ilsc_is_horizontal ::	InverseLineSegmentColoring ->	Char ->	Bool
ilsc_is_horizontal		ilsc							color	= i_s == i_e
	where
		endpoints 	= ilsc_get_endpoints ilsc color
		i_s			= fst (fst endpoints)
		i_e			= fst (snd endpoints)

--State

--τύπος δεδομένων
data State = State Grid PointColoring InverseLineSegmentColoring deriving (Show, Eq)

--πράξεις
state_get_grid :: 	State -> 			Grid
state_get_grid 		(State grid _ _) 	= grid


state_get_pnt_coloring :: 	State -> PointColoring
state_get_pnt_coloring		(State _ p_c _) = p_c


state_get_ilsc :: 	State -> 			InverseLineSegmentColoring
state_get_ilsc		(State _ _ ilsc)	= ilsc


readState :: String -> State
readState cs = State grid point_coloring inverse_line_segment_coloring
	where
		grid = grid_from_string cs (0,0)
		point_coloring = pnt_clr_from_string cs (0,0) (PointColoring Map.empty)
		ic_s = inv_clr_get_inverse_coloring grid (1,1) point_coloring (\new_v old_v -> old_v) (InverseColoring Map.empty)
		ic_e = inv_clr_get_inverse_coloring grid (1,1) point_coloring (\new_v old_v -> new_v) (InverseColoring Map.empty)
		inverse_line_segment_coloring = ilsc_get_inverse_coloring ic_s ic_e (InverseLineSegmentColoring Map.empty)

		
writeState :: 	State -> 	String
writeState 		state 		= pnt_clr_to_string (state_get_pnt_coloring state) (1,1) (state_get_grid state) []
		
		





{-	***********
	***Move****
	*********** -}



data Space = Space (Int,Int)	--free space to move 
data Direction  = Horizontal | Vertical deriving (Show,Eq)
data Move = Move Char Int deriving (Show, Eq)

{-successorMoves implementation-}
get_direction :: ((Int,Int),(Int,Int)) -> Direction
get_direction ((i1,j1),(i2,j2))
	| i1 - i2 == 0 = Horizontal
	| otherwise = Vertical

--divide the grid in two parts (left/right) for horizontal car, (up/down) for vertical car, our current car being the division point

--find (row,left), where left is the closest column on the left part of the grid, with neighbour car blocking
left_space :: PointColoring -> Int -> Int -> Int
left_space (PointColoring pc) row left 
	| left < 1 = left
	| new_color == (Just '.') = left_space (PointColoring pc) row (left-1) --free space
	| otherwise = left
	where 
		new_color = Map.lookup (row,left) pc

--find (up,column), where up is the closest row on the up part of the grid, with neighbour car blocking
up_space :: PointColoring -> Int -> Int -> Int
up_space (PointColoring pc) up column
	| up < 1 = up
	| new_color == (Just '.') = up_space (PointColoring pc) (up-1) column --free space
	| otherwise = up
	where 
		new_color = Map.lookup (up,column) pc

--find (row,right), where right is the closest row on the right part of the grid, with neighbour car blocking
right_space :: PointColoring -> Int -> Int -> Int -> Int
right_space (PointColoring pc) row right max
	| right > max = right
	| new_color == (Just '.') = right_space (PointColoring pc) row (right+1) max
	| otherwise = right
	where
		new_color = Map.lookup (row,right) pc

--find (down,column), where down is the closest row on the down part of the grid, with neighbour car blocking
down_space :: PointColoring -> Int -> Int -> Int -> Int
down_space (PointColoring pc) down column max
	| down > max = down
	| new_color == (Just '.') = down_space (PointColoring pc) (down+1) column max
	| otherwise = down
	where
		new_color = Map.lookup (down,column) pc

--possible moves moving forward (towards the point (x,y) of the grid with the greatest values x,y)
front_legalmove :: Space -> ((Int,Int),(Int,Int)) -> Char -> [Move]
front_legalmove (Space (_,x2)) (((i1,j1),(i2,j2))) color
	| get_direction ((i1,j1),(i2,j2)) == Horizontal = count_front_steps color j2 x2 1
	| otherwise = count_front_steps color i2 x2 1

--for every new sum of empty points in front of the car (*) a new move is made
--(*) towards the point (x,y) of the grid with the greatest values x,y
count_front_steps :: Char -> Int -> Int -> Int -> [Move]
count_front_steps color start end steps
	| end > (start+steps) = [Move color steps] ++ count_front_steps color start end (steps+1)
	| otherwise = []

--possible moves moving backwards (towards the point (x,y) of the grid with the lowest values x,y)
back_legalmove :: Space -> ((Int,Int),(Int,Int)) -> Char -> [Move]
back_legalmove (Space (x1,_)) (((i1,j1),(i2,j2))) color
	| get_direction ((i1,j1),(i2,j2)) == Horizontal = count_back_steps color j1 x1 (-1)
	| otherwise = count_back_steps color i1 x1 (-1)

--for every new (negative) sum of empty points on the back of the car (*) a new move is made
--(*) towards the point (x,y) of the grid with the lowest values x,y
count_back_steps :: Char -> Int -> Int -> Int -> [Move]
count_back_steps color start end steps
	| end < (start+steps) = [Move color steps] ++ count_back_steps color start end (steps-1)
	| otherwise = []

--creates a list of possible moves for each color 
legalmoves :: [(Char,((Int,Int),(Int,Int)))] -> (Int,Int) -> PointColoring -> [Move]
legalmoves [] _ _ = []
legalmoves ((color,edges):list) (h,w) (PointColoring pc) 
	| get_direction edges == Horizontal = (front_legalmove margin_x edges color) ++ (back_legalmove margin_x edges color) ++ legalmoves list (h,w) (PointColoring pc)  
	| otherwise = (front_legalmove margin_y edges color) ++ (back_legalmove margin_y edges color) ++ legalmoves list (h,w) (PointColoring pc) 
	where
		margin_x = Space (left_space (PointColoring pc) urow (lcolumn-1),right_space (PointColoring pc) urow (rcolumn+1) w)
		margin_y = Space (up_space (PointColoring pc) (urow-1) lcolumn,down_space (PointColoring pc) (drow+1) lcolumn h)
		urow = get_urow edges
		drow = get_drow edges
		lcolumn = get_lcolumn edges
		rcolumn = get_rcolumn edges

--getting the corresponding values from the ((row1,column1),(row2,column2)) in relation to the car's direction
get_urow :: ((Int,Int),(Int,Int)) -> Int
get_urow (((x1,_),_)) = x1

get_drow :: ((Int,Int),(Int,Int)) -> Int
get_drow ((_,(x2,_))) = x2

get_lcolumn :: ((Int,Int),(Int,Int)) -> Int
get_lcolumn (((_,j1),_)) = j1

get_rcolumn :: ((Int,Int),(Int,Int)) -> Int
get_rcolumn (((_),(_,j2))) = j2

--just get this part of State
get_inv_color :: State -> Map Char ((Int,Int),(Int,Int))
get_inv_color (State _ _ (InverseLineSegmentColoring inv_color)) =inv_color

--just get this part of State
get_pnt_color :: State -> PointColoring
get_pnt_color (State _ (PointColoring pnt_color) _) =(PointColoring pnt_color)

--just get this part of State
get_grid :: State -> (Int,Int)
get_grid (State (Grid (h,w)) _ _) = (h,w)

successorMoves_plain :: State -> [Move]
successorMoves_plain state = legalmoves (Map.toList (get_inv_color state)) (get_grid state) (get_pnt_color state)

successorMoves :: State -> [(Move,Int)]
successorMoves state = [(x,1) | x <- legalmoves (Map.toList (get_inv_color state)) (get_grid state) (get_pnt_color state)]


{-makeMove implementation-}
--update InverseLineSegmentColoring Map with new edges
move_car :: Int -> ((Int,Int),(Int,Int)) -> Maybe ((Int,Int),(Int,Int))
move_car steps ((i1,j1),(i2,j2))
	| i1 == i2 = Just ((i1,j1+steps),(i2,j2+steps))
	| otherwise = Just ((i1+steps,j1),(i2+steps,j2))

--get list of mappings (Map point color) and change the car's position
list_to_change :: [((Int,Int),Char)] -> Maybe ((Int,Int),(Int,Int)) -> Int -> Char -> [((Int,Int),Char)]
list_to_change list Nothing _ _ = list
list_to_change list (Just edges) steps color = pnts_to_change (get_direction edges) list edges steps color

--find the car's old position (points (x1,y2),(x2,y2)..) and make them empty (Map point '.')
--find the car's new position (points (x1',y2'),(x2',y2')..) and place car (Map point' color)
pnts_to_change :: Direction -> [((Int,Int),Char)] -> ((Int,Int),(Int,Int)) -> Int -> Char -> [((Int,Int),Char)]
pnts_to_change _ [] _ _ _ = []
pnts_to_change Horizontal (((x,y),ch):list) ((i1,j1),(i2,j2)) steps color
	| (x == i1) && (j1+steps <= y) && (y <= j2+steps) = ((x,y),color) : (pnts_to_change Horizontal list ((i1,j1),(i2,j2)) steps color) --points that are car's new position
	| (x == i1) && (j1 <= y) && (y <= j2) = ((x,y),'.') : (pnts_to_change Horizontal list ((i1,j1),(i2,j2)) steps color) --points where car was become empty
	| otherwise = ((x,y),ch) : (pnts_to_change Horizontal list ((i1,j1),(i2,j2)) steps color)
pnts_to_change Vertical (((x,y),ch):list) ((i1,j1),(i2,j2)) steps color
	| (y == j1) && (i1+steps <= x) && (x <= i2+steps) = ((x,y),color) : (pnts_to_change Vertical list ((i1,j1),(i2,j2)) steps color) --points that are car's new position
	| (y == j1) && (i1 <= x) && (x <= i2) = ((x,y),'.') : (pnts_to_change Vertical list ((i1,j1),(i2,j2)) steps color) --points where car was become empty
	| otherwise = ((x,y),ch) : (pnts_to_change Vertical list ((i1,j1),(i2,j2)) steps color)
	
makeMove :: State -> Move -> State
makeMove (State (Grid grid) (PointColoring pnt_color) (InverseLineSegmentColoring inv_color)) (Move color steps) = (State (Grid grid) (PointColoring pnt_color_new) (InverseLineSegmentColoring inv_color_new))
	where
		inv_color_new = Map.update (move_car steps) color inv_color
		pnt_color_new = Map.fromList (list_to_change (Map.toList pnt_color) edges steps color)
		edges = Map.lookup color inv_color





{-	********************
	*** PriorityQueue***
	******************** -}



--τύποι δεδομένων
data PriorityQueue t = PriorityQueue (Map Int [t]) deriving (Show)

{- 	Υλοποιούμε μια ουρά προτεραιότητας, όπου σε περίπτωση ισοβαθμίας προτεραιότητας, επιλέγουμε fifo.
	Στην ακραία περίπτωση όπου όλα τα στοιχεία έχουν την ίδια προτεραιότητα, η δομή εκφυλίζεται σε
	ουρά fifo. Διαφορετικά υλοποιούμε έναν Δυαδικό Σωρό.-}

--πράξεις
pq_insert ::	PriorityQueue t ->	Int ->	t ->	PriorityQueue t
pq_insert		(PriorityQueue pq)	value	elem	= PriorityQueue (Map.insertWith (\new old -> old ++ new) value [elem] pq) -- αν υπάρχει ήδη στοιχείο με ίδια προτεραιότητα
																															--βάλε το νέο στοιχείο στο τέλος

pq_insert_list :: 	PriorityQueue t ->	[(Int, t)] ->	PriorityQueue t
pq_insert_list		p_q					[]				= p_q

pq_insert_list		p_q					(e:es)			= pq_insert_list (pq_insert p_q (fst e) (snd e)) es


pq_get_min :: (Eq t) =>	PriorityQueue t	->	(t, PriorityQueue t)
pq_get_min				(PriorityQueue pq)
	|min_fifoQ_tail == []	= (min_fifoQ_head, PriorityQueue (Map.deleteMin pq)) 									--διαγράφουμε το πρώτο στοιχείο της fifo ουράς, αλλά δεν μένουν άλλα
	|otherwise				= (min_fifoQ_head, PriorityQueue (Map.insert min_key min_fifoQ_tail (Map.deleteMin pq))) --διαγράφουμε το πρώτο στοιχείο της fifo ουράς, αλλά μένουν και άλλα στοιχεία στην ουρά
	where
		(min_key, min_fifoQ)				= Map.findMin pq
		(min_fifoQ_head, min_fifoQ_tail)	= (head min_fifoQ, tail min_fifoQ)





{-	***************
	***Heuristic***
	*************** -}



{-Reference: https://github.com/atheed/UnblockMe-Solver/blob/master/unblockme.py-}

blocks_to_goal :: ((Int,Int),(Int,Int)) -> (Int,Int) -> Int
blocks_to_goal ((i1,j1),(i2,j2)) (h,w) = w - j2 

--calculate min cost of moves to free point (x,y) 
make_space :: [Move] -> State -> (Int,Int) -> Int -> Int
make_space [] _ _ min = min
make_space (m:moves) old_state (x,y) min 
	| (color == (Just '.')) && (steps < min) = make_space moves old_state (x,y) steps
	| otherwise = make_space moves old_state (x,y) min
	where
		color = Map.lookup (x,y) pnt_color_map
		pnt_color_map = (\(PointColoring map) -> map) (get_pnt_color (makeMove old_state m))
		steps = (\(Move k steps) -> (abs steps)) m

--for each car blocking the way find the blocking point in the way of the car "==" and its color
--first argument is points between "==" and goal
blocking_cars :: [(Int,Int)] -> PointColoring -> [((Int,Int),Char)]
blocking_cars pnt_list (PointColoring pnt_color) = [((x,y),color) | (x,y) <- pnt_list,let color = check_blocking_point (x,y) (PointColoring pnt_color), color /= '.']

--find color of car in the way of car "==" (point (x,y)) 
check_blocking_point :: (Int,Int) -> PointColoring -> Char
check_blocking_point (x,y) (PointColoring pnt_color) = (\(Just x) -> x) (Map.lookup (x,y) pnt_color)

--calculate all moves' min cost
blocking_cars_make_space :: [((Int,Int),Char)] -> [Move] -> State -> Int -> Int
blocking_cars_make_space [] _ _ _ = 0
blocking_cars_make_space (((x,y),color):block_list) moves state min = (make_space color_moves state (x,y) min) + blocking_cars_make_space block_list moves state min
	where
		color_moves = [(Move c steps) | (Move c steps) <- moves, c == color]

--creates a list of possible moves for each color assuming nothing is blocking the way
heur_legalmoves :: [(Char,((Int,Int),(Int,Int)))] -> (Int,Int) -> [Move]
heur_legalmoves [] _ = []
heur_legalmoves ((color,((i1,j1),(i2,j2))):list) (h,w) 
	| j1 == j2 = [(Move color (-n)) | n <- [1..(i1-1)]] ++ [(Move color (h-n)) | n <- [i2..(h-1)]] ++ heur_legalmoves list (h,w) 
	| otherwise = heur_legalmoves list (h,w) 

heur_successorMoves_plain :: State -> [Move]
heur_successorMoves_plain state = heur_legalmoves (Map.toList (get_inv_color state)) (get_grid state) 

heuristic :: State -> Int
heuristic (State (Grid (h,w)) (PointColoring pnt_color) (InverseLineSegmentColoring inv_color)) = (blocking_cars_make_space block_list (heur_successorMoves_plain state) state h) + (blocks_to_goal clean_edges (h,w))
	where
		state = State (Grid (h,w)) (PointColoring pnt_color) (InverseLineSegmentColoring inv_color)
		block_list = blocking_cars [(row,y) | y <- [start..w]] (PointColoring pnt_color)
		edges = Map.lookup '=' inv_color
		clean_edges = (\(Just ((x,j1),(i2,j2))) -> ((x,j1),(i2,j2))) edges
		row = (\(Just ((x,j1),(i2,j2))) -> x) edges
		start = ((\(Just ((i1,j1),(i2,y))) -> y) edges)+1




{-	***********
	***Solve***
	*********** -}



finalState :: 	State -> 	Bool
finalState		state		= (grid_touches_the_right_edge grid end)
	where
		ilsc 	= state_get_ilsc state
		end 	= snd (ilsc_get_reds_endpoints ilsc)
		grid	= state_get_grid state
		

snd_of_list ::	[(t1, t2)] ->	[t2]
snd_of_list		ls				= (Prelude.map (\x -> (snd x)) ls)


fst_of_list :: 	[(t1, t2)] ->	[t1]
fst_of_list		ls				= (Prelude.map (\x -> (fst x)) ls)


join_lists ::	[t1] ->	[t2] ->	[(t1, t2)]
join_lists		[]		[]		= []

join_lists		(l:ls)	(k:ks)	= (l, k) : (join_lists ls ks)


--Solve BestFirst

solveBestFirst :: 	PriorityQueue (State, [(Move, Int)]) ->	(State -> Int) -> [Move]
solveBestFirst		p_q										h_func
	|finalState state	= reverse (fst_of_list path)
	|otherwise			= solveBestFirst (pq_insert_list unvisited_nodes discovered_nodes) h_func
	where
		
		(min_node, unvisited_nodes)	= pq_get_min p_q --κεφαλή (κόμβος στον οποίο βρισκόμαστε) και υπόλοιπη ουρά προτεραιότητας (υπόλοιποι κόμβοι που έχουν ανακαληφθεί)
		
		(state, path)				= min_node --η κεφαλή έχει την δυάδα (κατάσταση, )
		
		succ_Moves					= successorMoves state
		
		new_states					= Prelude.map (makeMove state) (fst_of_list succ_Moves)
		
		new_paths					= [new_move : path | new_move <- succ_Moves]
		
		discovered_nodes			= [((h_func new_state) + (sum (snd_of_list new_path)), (new_state, new_path)) | (new_state, new_path) <- (join_lists new_states new_paths)]

{-(*)	η στρατηγική μας διακλαδίζεται στις πιθανές υλοποιήσιμες κινήσεις στην νέα κατάσταση που βρεθήκαμε-}


solve :: 	State ->	[Move]
solve		state		= solveBestFirst ( PriorityQueue ( Map.fromList [ (0, [(state, [])]) ] ) ) (\x -> 0) 


solve_astar ::	State ->	[Move]
solve_astar		state		= solveBestFirst ( PriorityQueue ( Map.fromList [ (0, [(state, [])]) ] ) ) heuristic



{-	***********
	***Debug***
	*********** -}


printSolution s [] = putStrLn (writeState s)

printSolution s (m : ms) = do {putStrLn (writeState s);
								printSolution (makeMove s m) ms}
