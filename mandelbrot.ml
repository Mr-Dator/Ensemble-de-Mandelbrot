#load "graphics.cma";;
open Graphics;;

let pixelColor a b =
   let x = ref a and y = ref b and n = ref 0 in
      while !x *. !x +. !y *. !y <= 4. && !n <= 128  do
         let t = ref (!x *. !x -. !y *. !y +. a) in
            y := 2. *. !x *. !y +. b;
            x := !t;
            n := !n + 1
      done;
      let c = 2 * !n in rgb (c) (c) c;;

let t = 400;;

let screen = Array.make_matrix t t (rgb 0 0 0) in

let mandelBrot a b l =
	
	let cx, cy = a -. (l/. 2.), b +. (l/. 2.) in
	for i = 0 to t-1 do
		for j = 0 to t-1 do
			let x = cx +. (float_of_int i *. l /. float_of_int t  ) in
			let y = cy -. (float_of_int j *. l /. float_of_int t )in
			screen.(j).(i) <- pixelColor x y;
		done;
	done;
	draw_image (make_image screen) 0 0
	in


let x, y, l = ref 0., ref 0., ref 5. in
open_graph (" " ^ string_of_int t ^ "x" ^ string_of_int t);
set_window_title "Ensemble de MandelBrot";
mandelBrot !x !y !l ;

let rec main () =
	let s = Graphics.read_key () in
	match s with
	'w' -> y := !y -. 0.1 *. !l; mandelBrot !x !y !l; main ()
	|'s' -> y := !y +. 0.1 *. !l; mandelBrot !x !y !l; main ()
	|'a' -> x := !x -. 0.1 *. !l; mandelBrot !x !y !l; main ()
	|'d' -> x := !x +. 0.1 *. !l; mandelBrot !x !y !l; main ()
	|'i' -> l := 0.5 *. !l; mandelBrot !x !y !l; main ()
	|'j' -> l := 0.02; mandelBrot !x !y !l; main ()
	|'x' -> close_graph ()
	|_ -> main ()
	in

main ();;

2;;
