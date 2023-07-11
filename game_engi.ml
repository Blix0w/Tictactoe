(* to handle the game parts : changing symbol on a case, finding next move for computer, detecting win, etc. *)

let grid_size = 600
let blank = 0
let player1 = 1
let player2 = 2
let curr_player = ref player1

let grid = [|player1; blank; blank;
			blank; blank; blank;
			blank; blank; blank|]

(* gives the id of the square (from 0 to 8) corresponding to the position *)
let pos_to_square_id x y = 
	if x > grid_size || y > grid_size then -1 else
	let xtmp = ref 0 and ytmp = ref 0 in
	((if x <= (grid_size/3) then xtmp := 0
	else if x <= 2*grid_size/3 then xtmp := 1
	else xtmp := 2);
	(if y<= (grid_size/3) then ytmp := 0
	else if y <= 2*grid_size/3 then ytmp := 1
	else ytmp := 2);
	!xtmp + 3 * !ytmp)

let get_symbol id = grid.(id)

let opponent color = 
	if color = player1 then player2 else player1

let play_square x y = 
	let id = pos_to_square_id x y in 
	Printf.printf "Position du clic : (%d, %d) / id = %d" x y id; print_newline ();
	if id > 0 then begin
		if grid.(id) = blank then 
			begin grid.(id) <- !curr_player; curr_player := opponent !curr_player end
		end

