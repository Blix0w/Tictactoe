open Graphics
open Game_engi

let grid_size = 600
let scoreboard_width = 300


(* draws the 3x3 grid *)
let draw_grid () =
	let segments = [|grid_size/3 - 1, 0, grid_size/3 - 1, grid_size - 1; 2*grid_size/3 - 2, 0, 2*grid_size/3 - 2, grid_size - 1;
					 0, grid_size/3 - 1, grid_size - 1, grid_size/3 - 1; 0, 2*grid_size/3 - 2, grid_size - 1, 2*grid_size/3 - 2|]
		in set_line_width 3;
		set_color black;
		draw_segments segments


let draw_symbols () =
	for i=0 to 2 do
		for j=0 to 2 do
			(if grid.(i+3*j) = player1 then Printf.printf "joueur1"
			else if grid.(i+3*j) = player2 then Printf.printf "joueur2"
			else Printf.printf "blanc");
		done
	done;
	print_newline ()

let draw_scoreboard () = 
	()


(* restart button *)
let draw_toolbar () = 
	()

(* to handle events such as clicks on the grid *)
let rec interactive () =
  let event = wait_next_event [Key_pressed; Button_down] in
  if event.keypressed && event.key == 'q' then exit 0
	else if event.keypressed then begin print_char event.key; print_newline () end
  else if event.button then begin (Game_engi.play_square event.mouse_x event.mouse_y); draw_symbols () end;
  interactive ()



let init () =
	Graphics.open_graph "";
	resize_window (grid_size+scoreboard_width) grid_size;
	Graphics.set_window_title "Tic-tac-toe";
	Graphics.set_color (Graphics.rgb 190 190 190);
	draw_grid ();
	draw_scoreboard ();
	interactive ()

let _ = init ()