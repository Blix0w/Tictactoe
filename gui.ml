open Graphics
open Game_engi

(* ------ Fichier game_engi.ml------ *)

let grid_size = 600
let blank = 0
let player1 = 1
let player2 = 2

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


let grid_size = 600
let scoreboard_window_width = 300
let scoreboard_width = 200
let scoreboard_height = 300
let text_padding = 10 (* the padding between the drawn text and the scoreboard lines *)

(* ------ CODE ------ *)

let grid = [|blank; blank; blank;
			blank; blank; blank;
			blank; blank; blank|]

let curr_player = ref player1
let scores = [|0;0;0|]  (* player1 wins, player2 wins, draws *)
let game_is_won = ref false

let get_score_str id = string_of_int scores.(id)

let incr_score id = 
	scores.(id) <- scores.(id) + 1

let reset_current_game () = 
	for i=0 to 8 do grid.(i) <- blank done;
	curr_player := player1;
	game_is_won := false


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

(* plays symbol on the board, updates scoreboard *)
let play_square x y board color = 
	let id = pos_to_square_id x y in 
	Printf.printf "Position du clic : (%d, %d) / id = %d" x y id; print_newline ();
	if id >= 0 && not(!game_is_won) then begin
		if board.(id) = blank then begin 
			if is_winning_move id color board then begin Printf.printf "Victoire du joueur %d" color; print_newline (); 
				scores.(color) <- scores.(color) + 1; game_is_won := true end;
			board.(id) <- color end
		end
	else if x >= grid_size + scoreboard_window_width/2 - scoreboard_width/4 && x <= grid_size + scoreboard_window_width/2 + scoreboard_width/4
	&& y >= grid_size/3 - scoreboard_width/2 && y <= grid_size/3 then reset_current_game ()



(* ------ fin fichier game_engi.ml *)

(* ------     elagage alpha beta      ------ *)
let cases_libres board =
	let libres = ref [] in
	for i=0 to 8 do
		if board.(i) = blank then libres := i::(!libres)
	done;
	!libres

let heuristique board color = 
	(* if it's a winning board *)
	if board.(0) = color && board.(0) = board.(1) && board.(1) = board.(2)
		|| board.(3) = color && board.(4) = board.(3) && board.(3) = board.(5)
		|| board.(6) = color && board.(6) = board.(7) && board.(6) = board.(8)
		|| board.(0) = color && board.(0) = board.(3) && board.(0) = board.(6)
		|| board.(1) = color && board.(1) = board.(4) && board.(1) = board.(7)
		|| board.(2) = color && board.(2) = board.(5) && board.(2) = board.(8)
		|| board.(0) = color && board.(0) = board.(4) && board.(0) = board.(8)
		|| board.(2) = color && board.(2) = board.(4) && board.(2) = board.(6)
	then 10
	(* if 2 same symbols in a row/col/diag and 3rd is blank *)
	else if board.(0) = blank && board.(1) = color && board.(2) = color
		|| board.(0) = color && board.(1) = blank && board.(2) = color
		|| board.(0) = color && board.(1) = color && board.(2) = blank

		||board.(3) = blank && board.(4) = color && board.(5) = color
		|| board.(3) = color && board.(4) = blank && board.(5) = color
		|| board.(3) = color && board.(4) = color && board.(5) = blank

		||board.(6) = blank && board.(7) = color && board.(8) = color
		|| board.(6) = color && board.(7) = blank && board.(8) = color
		|| board.(6) = color && board.(7) = color && board.(8) = blank

		||board.(0) = blank && board.(3) = color && board.(6) = color
		|| board.(0) = color && board.(3) = blank && board.(6) = color
		|| board.(0) = color && board.(3) = color && board.(6) = blank

		||board.(1) = blank && board.(4) = color && board.(7) = color
		|| board.(1) = color && board.(4) = blank && board.(7) = color
		|| board.(1) = color && board.(4) = color && board.(7) = blank

		||board.(2) = blank && board.(4) = color && board.(8) = color
		|| board.(2) = color && board.(4) = blank && board.(8) = color
		|| board.(2) = color && board.(4) = color && board.(8) = blank

		||board.(0) = blank && board.(4) = color && board.(8) = color
		|| board.(0) = color && board.(4) = blank && board.(8) = color
		|| board.(0) = color && board.(4) = color && board.(8) = blank

		||board.(2) = blank && board.(4) = color && board.(6) = color
		|| board.(2) = color && board.(4) = blank && board.(6) = color
		|| board.(2) = color && board.(4) = color && board.(6) = blank
	then 5
	else 0

(*
let rec elagage board heuristique couleur depth lim_depth couleur_actu alpha beta =
	if depth > lim_depth then heuristique board else
	let libres = case libres board in 
		match libres with
		|[] -> print_string "\nfeuille\n"; if couleur_actu = couleur then 10 else 0 (* cest une feuille *) (* renvoie 1 si le joueur couleur gagne, -1 si l'autre joueur gagne *)
		|hd::tl when couleur = couleur_actu -> print_string "\njoueur 1\n";
			let val_fils = List.map (fun (i,j,nx,ny,dx,dy) -> (elagage (jouer_coup board couleur_actu i j nx ny dx dy) heuristique couleur (depth+1) lim_depth (opponent couleur_actu))) libres in max_list val_fils
		|hd::tl when couleur = (opponent couleur_actu) -> print_string "\njoueur 2\n";
			let val_fils = List.map (fun (i,j,nx,ny, dx, dy) -> elagage (jouer_coup board couleur_actu i j nx ny dx dy) heuristique couleur (depth+1) lim_depth (opponent couleur_actu)) libres in min_list val_fils
*)

(* draws the 3x3 grid *)
let draw_grid () =
	let segments = [|grid_size/3 - 1, 0, grid_size/3 - 1, grid_size - 1; 2*grid_size/3 - 2, 0, 2*grid_size/3 - 2, grid_size - 1;
					 0, grid_size/3 - 1, grid_size - 1, grid_size/3 - 1; 0, 2*grid_size/3 - 2, grid_size - 1, 2*grid_size/3 - 2|]
		in set_color black; draw_segments segments


let draw_symbols () =
	for j=2 downto 0 do
		for i=0 to 2 do
			(if grid.(i+3*j) = player1 then Printf.printf "X|"
			else if grid.(i+3*j) = player2 then Printf.printf "O|"
			else Printf.printf " |");
		done;
		Printf.printf "\n"
	done;
	print_newline ();
	for i=0 to 2 do
		for j=0 to 2 do
			if grid.(i+3*j) = blank then begin (* on remplit la case de blanc *)
				set_color background;
				fill_rect (1 + i*grid_size/3) (1 + j*grid_size/3) (grid_size/3 - 1) (grid_size/3 - 1) end
			else 
				let x0 = grid_size/6 * (2*i+1) and y0 = grid_size/6 * (2*j+1) in
				if grid.(i+3*j) = player1 then begin (* si c'est joueur1 : on dessine une croix *)
					set_color black;
					let segm_croix = [|x0 - grid_size/12, y0 - grid_size/12, x0 + grid_size/12, y0 + grid_size/12;
									   x0 - grid_size/12, y0 + grid_size/12, x0 + grid_size/12, y0  - grid_size/12|] in
						draw_segments segm_croix end
				else begin  (* si c'est joueur2 : on dessine un cercle *)
					set_color black;
					draw_circle x0 y0 (grid_size/12)
				end
		done
	done

(* x,y is the bottom left of the scoreboard *)
let draw_scoreboard () = 
	(* drawing the scoreboard itself *)
	let x = grid_size + scoreboard_window_width/2 - scoreboard_width/2 and y = 2*grid_size/3 - scoreboard_height/2 in
	(* cleaning old scoreboard *)
	set_color background;
	fill_rect x y scoreboard_width scoreboard_height;
	(* drawing new scoreboard *)
	set_color black;
	draw_rect x y scoreboard_width scoreboard_height;
	let lines = [|x,y,x+scoreboard_width,y; 
				  x,y+scoreboard_height/3, x+scoreboard_width,y+scoreboard_height/3;
				  x,y+2*scoreboard_height/3, x+scoreboard_width,y+2*scoreboard_height/3|] in
	draw_segments lines;
	(* drawing the text : names and scores *)
	let a,b = text_size "Draws" in 
	moveto (x+text_padding) (y+scoreboard_height/3 - text_padding - b);
	draw_string "Draws";
	rmoveto (-a) (-scoreboard_height/6);
	draw_string (get_score_str 0);
	let a,b = text_size "Player2 wins" in 
	moveto (x+text_padding) (y+2*scoreboard_height/3 - text_padding - b);
	draw_string "Player2 wins";
	rmoveto (-a) (-scoreboard_height/6);
	draw_string (get_score_str 1);
	let a,b = text_size "Player1 wins" in 
	moveto (x+text_padding) (y+scoreboard_height - text_padding - b);
	draw_string "Player1 wins";
	rmoveto (-a) (-scoreboard_height/6);
	draw_string (get_score_str 2)



(* restart button *)
let draw_toolbar () = 
	let x = grid_size + scoreboard_window_width/2 - scoreboard_width/4 and y = grid_size/3 - scoreboard_width/2 in
	draw_rect x y (scoreboard_width/2) (scoreboard_width/2)

(* to handle events such as clicks on the grid *)
let rec interactive () =
  let event = wait_next_event [Key_pressed; Button_down] in
  if event.keypressed && event.key == 'q' then exit 0
	else if event.keypressed then begin print_char event.key; print_newline () end
  else if event.button then begin (play_square event.mouse_x event.mouse_y grid !curr_player); curr_player := opponent !curr_player;
  	draw_symbols (); draw_grid (); draw_scoreboard (); end;
  interactive ()



let init () =
	Graphics.open_graph "";
	resize_window (grid_size+scoreboard_window_width) grid_size;
	Graphics.set_window_title "Tic-tac-toe";
	Graphics.set_color (Graphics.rgb 190 190 190);
	set_line_width 3;
	set_text_size 30;
	set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
	set_color black;
	draw_grid ();
	draw_scoreboard ();
	draw_toolbar ();
	interactive ()

let _ = init ()