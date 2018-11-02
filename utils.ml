
open Graphics;;

(**
  * clamp v mn mx
  * clamp a value v between a maximun mx and a minimum mn
  *)
let clamp v mn mx = min mx (max mn v);;

let rec power a n =
  if n = 0 then 1
  else let sub = power a (n / 2) in
    if n mod 2 = 1 then
      a * sub * sub
    else sub * sub;;

type button = {text : string; x : int; y : int; size_x : int; size_y : int; run : (unit -> unit); cost : int; mutable usable : bool; force_usable : bool; description : string list};;

let is_hover x y b = b.x <= x && b.y <= y && b.x + b.size_x >= x && b.y + b.size_y >= y;;

let check_buttons x y = 
  List.iter (function b -> 
  if b.usable && is_hover x y b then b.run ()
  else ());;

let description_size d = List.fold_left (fun (x,y) (x2,y2) -> (max x x2,y+1+y2)) (0,0) (List.map (fun s -> text_size s) d);;

let rec draw_desc x y = function
  | [] -> ()
  | s::r -> let (_, ty) = text_size s in
    moveto x (y-ty);
    draw_string s;
    draw_desc x (y - ty - 1) r
 
let get_relative_pos x y w h = 
  let get x w f = 
    if x + w > f() then let x = x - w - 30 in
      if x < 0 then 0 else x
    else x in
  (get x w size_x, get y h size_y);;
