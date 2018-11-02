
open Graphics;;
open Utils;;

let gray = rgb 177 177 177
and light_gray = rgb 220 220 220;;

open_graph " 800x600";;
set_text_size 40;;
set_window_title "Funland";;
auto_synchronize false;;

let buttons = ref ( [] : button list )
and last_key = ref 'a'
and old_x = ref (-1)
and old_y = ref (-1)
and points = ref 50000
and gain = ref 0
and menu = ref 0
and money = "$"
and max_money = 100000000

and tier_0 = ref 1
and tier_1 = ref 0
and powe_1 = 5
and tier_2 = ref 0
and powe_2 = 25
and tier_3 = ref 0
and powe_3 = 100
and tier_4 = ref 0
and powe_4 = 250
and tier_5 = ref 0
and powe_5 = 1000
and tier_6 = ref 0
and powe_6 = 2500
and tier_7 = ref 0
and powe_7 = 10000;;
let tiers = [tier_1; tier_2; tier_3; tier_4; tier_5; tier_6; tier_7]
and powes = [powe_1; powe_2; powe_3; powe_4; powe_5; powe_6; powe_7];;

let add_button t _x _y w h r _cost _force_usable (_description : string list)=
  let b = {text=t; x = _x; y = _y; size_x = w; size_y = h; 
  run = (function () -> (
  points:= !points - _cost;
  r())); 
  cost = _cost; usable = true; force_usable = _force_usable; description = _description} in (* TODO : Description ! *)
  buttons:= b::!buttons;
  b;;


let rec init () =
  old_x := size_x ();
  old_y := size_y ();
  let rec calc_gain tiers powes = match (tiers,powes) with
    | (tier::t,powe::p) -> (!tier * powe) + calc_gain t p
    | _ -> 0 in
  gain:= calc_gain tiers powes;
  buttons := ( [] : button list ); (* Vide les boutons *)
  (function
  (* Création des nouveaux boutons *)
  | 0 -> (
    let _ = add_button "Menu" (size_x() - 198) 0 196 40 (function () -> menu := 1;()) 0 true [] in 
    let _ = add_button ("Main de fun T"^(string_of_int (!tier_0))) 0 0 (size_x() - 200) 40 (function () ->()) (0 - !tier_0 * 3) true [] in
    ()
  )
  | 1 -> (
    (* TODO: Créer facile d'objet *)
      let usine nom x y tier factor d =
      add_button (nom^" "^(if !tier < 8 then "T"^string_of_int (!tier + 1) else "Max")) x y 128 128 (function () -> (tier:=!tier+1);init ()) (factor * power 2 (!tier + 1)) (!tier < 8) ("Augmenter le niveau de 1"::d) in
      let auto_usine nom x y tier factor d powe = usine nom x y tier factor ([((string_of_int (powe * !tier))^"+"^(string_of_int powe)^" "^money^"/t");"------------"]@d) in
      let _ = usine "Main de fun" (size_x() / 2 - 304) (size_y() / 2 + 16) tier_0 40 ["Ajoute 3"^money^" par main"] in
      
      let _ = auto_usine "Lan de Francis" (size_x() / 2 - 144) (size_y() / 2 + 16) tier_1 240 ["Un groupe d'amis jouent ensemble";"dans la cave de Francis ainsi ils";"augmentent le fun du monde!"] powe_1 in
      
      let _ = auto_usine "Cinema" (size_x() / 2 + 16) (size_y() / 2 + 16) tier_2 480 ["Un cinema oui, mais un cinema";"gratuit, OUI OUI OUI !";"Vous creez un cinema gratuit pour le monde";"passant que des bons films augmentant"; "ainsi le fun du monde"] powe_2 in
      
      let _ = auto_usine "Usine de Pizza" (size_x() / 2 + 176) (size_y() / 2 + 16) tier_3 960 ["Une usine de pizza produit du fun";"parce que au fond, tout le monde";"aime les pizzas!"] powe_3 in
      
      let _ = auto_usine "Jeu AAA" (size_x() / 2 - 304) (size_y() / 2 - 144) tier_4 3000 ["Production d'un jeu AAA comme";"il en existe des centaines, mais";"personne ne remarque et l'achete";"moins de fun par personne est";"produit mais plus de gens sont";"touches donc le fun augmente"; "bien plus";"Le gout des bons jeux se perdra (mais";"on s'en fiche...)"] powe_4 in
      
      let _ = auto_usine "Chaine de TV" (size_x() / 2 - 144) (size_y() / 2 - 144) tier_5 7000 ["Achat d'une chaine de TV pour creer";"des emissions dignes de TPMP ou les";"ch'tits a <ville random> ! Les gens regardent";"en masse augmentant le fun";"L'intelligence se perdra (mais qui";"ca va interesser...)"] powe_5 in
      
      let _ = auto_usine "Police funique" (size_x() / 2 + 16) (size_y() / 2 - 144) tier_6 18000 ["Une police dirige des recherches pour";"trouver les gens sans fun pour les";"lobotomiser pour qu'ils ont plus";"de fun";"Le monde sera sous la crainte de";"se faire emmener par la police"] powe_6 in
      
      let _ = auto_usine "Conversion" (size_x() / 2 + 176) (size_y() / 2 - 144) tier_7 500000 ["Conversion du monde en monde robotique";"les robots sont stimules artificiellement";"afin d'Augmenter leurs fun";"Le fun sera peut-etre present mais";"les emotions disparaitrons..."] powe_7 in
      
      let _ = add_button "Retour" (size_x() - 198) 0 196 40 (function () -> menu := 0;()) 0 true [] in ())
  | _ -> ()
  ) !menu;
    List.iter (fun b -> (b.usable <- (b.cost <= !points && b.force_usable);())) !buttons;
  ()
;;

let draw x y = 
  clear_graph ();
  set_color light_gray;
  fill_rect 0 0 (size_x()) (size_y() - 40);
  set_color gray;
  fill_rect 0 (size_y() - 40) (size_x()) (size_y());
  set_color (if !points = max_money then yellow else green);
  fill_rect 0 (size_y() - 40) ((size_x() * !points) / max_money) (size_y());
  let text = "Fun du monde: " ^ (string_of_int !points) ^ money ^ 
    (if !gain <> 0 then " (+"^(string_of_int !gain)^"/t)" else "") in
  let (tx, ty) = text_size text in
  moveto (ty/2) (size_y() - 20 - ty/2);
  set_color black;
  draw_string text;
  let descr = ref ( [] : string list ) in
  List.iter (function b -> 
    let h = is_hover x y b in
    let c11 = if b.usable then red else gray in
    let c1 = if h then c11 else white
    and c2 = if h then white else c11 in
    (if h then (descr:=b.description @ !descr;()) else ());
    set_color c1;
    fill_rect b.x b.y b.size_x b.size_y;
    set_color c2;
    draw_rect b.x b.y b.size_x b.size_y;
    let (w,h) = text_size b.text in
    (if b.force_usable && b.cost <> 0 then (
      let text = ((if b.cost < 0 then "+" else "")^(string_of_int (-1 * b.cost)) ^ money) in
      let (w2,h2) = text_size text in
      moveto (b.x + b.size_x/2 - w2/2) (b.y + b.size_y/2 - h/2 - h2/2);
      draw_string text;
      moveto (b.x + b.size_x/2 - w/2) (b.y + b.size_y/2 + h/2 - h/2 + 1);
      ()
    ) else 
      (moveto (b.x + b.size_x/2 - w/2) (b.y + b.size_y/2 - h/2);()));
    draw_string b.text;
    ()
  ) !buttons;
  (function 
    | [] -> ()
    | d -> (
      let (sx, sy) = description_size d in
      let (sx, sy) = (sx + 17, sy + 10) in
      let (x, y) = get_relative_pos (x+15) (y-15) (sx) (sy) in
      set_color white;
      fill_rect x y sx sy;
      set_color red;
      draw_rect x y sx sy;
      draw_desc (x+7) (y+sy-5) d;
      ())
  ) !descr;
  synchronize ();
  ()
;;
(fun mf kf tf ->
  let tick_wait = ref 0.0
  and last_mouse = ref false in
  (while true do
    let (cx, cy) = mouse_pos () in
    (if (not (size_x() = !old_x && size_y() = !old_y)) then
      init ()
    else ());
    draw cx cy;
    (if button_down () then
      (if not !last_mouse then
        (last_mouse:=true;
        mf cx cy;
        ())
      else ())
    else
      (last_mouse:=false;())
    );
    (if key_pressed () then
        (kf (read_key ());
        ())
    else ()
    );
    (if Sys.time() >= !tick_wait +. 0.05 then
      (tick_wait := Sys.time();tf ();())
    else ());
    ()
    
done))
(* Mouse click *)
(fun cx cy -> 
  let m = !menu in 
    (check_buttons cx cy !buttons;
    (if m <> !menu then (init (); ()) else ());
    List.iter (fun b -> (b.usable <- (b.cost <= !points && b.force_usable);())) !buttons;
    ()
    )
) 
(* Key press *)
(fun k -> ())
(* Tick *)
(fun () -> ( (* tick *)
  
  points:= !points + !gain;
  (if !points > max_money then (points:= max_money;()) else ());
  init();
()));;



