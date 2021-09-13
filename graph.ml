
module Positions = Set.Make(Int);;

type t = Positions.t array array;;

let (create:unit->t) = fun () -> Array.make_matrix Symbol.nb_syms Symbol.nb_syms Positions.empty;;

let get_edge g a b = g.(Symbol.to_int a).(Symbol.to_int b);;

let add_edge g a b i =
  let inta = Symbol.to_int a and intb = Symbol.to_int b in
  let newedge = Positions.add i (get_edge g a b) in
  begin
    g.(inta).(intb) <- newedge;
    g.(intb).(inta) <- newedge
  end;;

let fold_over_connected g f x s =
  let neighbourhood = g.(Symbol.to_int s) in
  let func x s =
    if Positions.is_empty neighbourhood.(Symbol.to_int s)
    then x
    else f x s;
  in Symbol.fold func x;;

let print_path g oc symli = match symli with
    [] -> ()
   |s0::t ->
     let rec rec_print s tail = match tail with
         [] -> ()
        |s'::t -> begin
            output_value oc g.(Symbol.to_int s).(Symbol.to_int s');
            rec_print s' t;
          end;
     in rec_print s0 t;;
  

