let graph_of_known_cipher a b =
  let g = Graph.create() in
  for i=0 to (String.length a) - 1 do
    Graph.add_edge g (Symbol.of_char a.[i]) (Symbol.of_char b.[i]) i
  done;
  g;;

let init_paths () =
  let func li x = (Path.singleton x)::li in
  Symbol.fold func [];;

type t = Path.t list;;

let mem = List.mem;;

let cycles g =
  let rec rec_cycles elt_paths resu = match elt_paths with
      [] -> resu
     |p::t ->
       let s = Path.source p and finalelt = List.hd (Path.rev_path p) in
       let func (elt_paths, resu) sym =
         if sym == s
         then begin
             let path = Path.rev_path p in
             if (List.length path) = 2
             then (elt_paths, resu)
             else begin
                 let sndelt = List.hd (List.tl (List.rev path)) in
                 if (Symbol.to_int sndelt) < (Symbol.to_int finalelt)
                 then (elt_paths, (Path.snoc p sym)::resu)
                 else (elt_paths, resu)
               end
           end
         else begin
             if (Path.mem p sym) || (Symbol.to_int s) > (Symbol.to_int sym)
             then (elt_paths, resu)
             else ((Path.snoc p sym)::elt_paths, resu)
           end
       in let elt_paths,resu = Graph.fold_over_connected g func (t,resu) finalelt in
          rec_cycles elt_paths resu
  in rec_cycles (init_paths()) [];;

let to_paths c = c 

let iter_over_cycles g f =
  let cycles = cycles g in
  let rec iter cycles = match cycles with
      [] -> ()
     |c::t -> f g c;
              iter t;
  in iter cycles;;

let rec print_rev_path p = match p with
          [] -> ()
         |h::t-> print_rev_path t;Printf.printf "%c " (Symbol.to_char h);;


let () =
  if Filename.basename Sys.argv.(0) = "cycles" then begin
      let g = graph_of_known_cipher Sys.argv.(1) Sys.argv.(2)
      and nb_cycles = ref 0 and nb_expended_cycles = ref 0
      in let count_cycles _ _ =
           incr nb_cycles
         and count_expended_cycles g c =
           let path = Path.rev_path c in
           let rec multi_expended_cycles path = match path with
               [] -> 0
              |[_] -> 1
              |s::(s'::t) -> let nb_sub_cycles = multi_expended_cycles (s'::t)
                             in nb_sub_cycles *
                                  Graph.Positions.cardinal (Graph.get_edge g s s')
           in nb_expended_cycles := !nb_expended_cycles + (multi_expended_cycles path)
         in
         iter_over_cycles g count_cycles;
         iter_over_cycles g count_expended_cycles;
         Printf.printf "Nb cycles avant exp: %d\n" !nb_cycles;
         if !nb_cycles < 20
         then begin
             let print_cycle _ c =
               print_rev_path (Path.rev_path c);
               Printf.printf "\n";
             in iter_over_cycles g print_cycle;
           end;
         Printf.printf "Nb cycles après exp: %d\n" !nb_expended_cycles;
            
    end
