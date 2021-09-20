
type t = ((bool Symbol.Map.t) * bool ) Symbol.Map.t;;

let top () =
  let fullset = Symbol.Map.make true in
  Symbol.Map.make (fullset, false);;

let possible p s s' = Symbol.Map.get (fst (Symbol.Map.get p s)) s';;

let possibles p s =
  let check_sym sym_li s' =
    if possible p s s'
    then s'::sym_li
    else sym_li
  in Symbol.fold check_sym [];;

exception Impossible;;

let only_symbol mx =
  let true_symb x z  =
    if Symbol.Map.get mx z
    then z
    else x
  in Symbol.fold true_symb Symbol.a;;

let rec remove_unit_op p x y =
  let mx,boox = Symbol.Map.get p x and my,booy = Symbol.Map.get p y
  in Symbol.Map.set my x false;
     Symbol.Map.set mx y false;
     Symbol.Map.set p y (my,booy);
     Symbol.Map.set p x (mx,boox);
     let posx = possibles p x and posy = possibles p y in
     if List.length posx = 0 || List.length posy = 0
     then raise Impossible
     else begin
         if List.length posx = 1
         then constrain p x;
         if List.length posy = 1
         then constrain p y;
       end
     
and constrain p x =
  let mx = fst(Symbol.Map.get p x) in
  Symbol.Map.set p x (mx, true);
  let z = only_symbol mx in
  let remove_except_x x z y =
    if y <> x then remove_unit_op p y z
  in Symbol.iter (remove_except_x x z);;


let remove_assoc p x y =
  remove_unit_op p x y;
