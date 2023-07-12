(* to handle the game parts : changing symbol on a case, finding next move for computer, detecting win, etc. *)

open Gui

(* ------ CONSTANTS ------ *)

let grid_size = 600
let blank = 0
let player1 = 1
let player2 = 2
let curr_player = ref player1

let winLines = [|
	[|[|1;2|]; [|3;6|]; [|4;8|]|];
	[|[|0;2|]; [|4;7|]|];
	[|[|0;1|]; [|4;6|]; [|5;8|]|];
	[|[|0;6|]; [|4;5|]|];
	[|[|1;7|]; [|3;5|]; [|0;8|]; [|6;2|]|];
	[|[|3;4|]; [|2;8|]|];
	[|[|0;3|]; [|2;4|]; [|7;8|]|];
	[|[|1;4|]; [|6;8|]|];
	[|[|6;7|]; [|0;4|]; [|2;5|]|]
|]

exception IsWinning


(* ------ CODE ------ *)

let grid = [|blank; blank; blank;
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

let opponent color = 
	if color = player1 then player2 else player1


(* check if the board is currently a winning position for the given player *)
let is_winning_move move player board = 
	let is_winning = ref false in
			for i=0 to (Array.length (winLines.(move)) -1) do
				let line = winLines.(move).(i) in
					if player = board.(line.(0)) && player = board.(line.(1)) then
					begin is_winning := true; Printf.printf "victoire par %d %d" line.(0) line.(1) end
			done;
			!is_winning

let play_square x y = 
	let id = pos_to_square_id x y in 
	Printf.printf "Position du clic : (%d, %d) / id = %d" x y id; print_newline ();
	if id >= 0 then begin
		if grid.(id) = blank then begin 
			if is_winning_move id !curr_player grid then begin Printf.printf "Victoire du joueur %d" !curr_player; print_newline () end;
			grid.(id) <- !curr_player; curr_player := opponent !curr_player end
		end
