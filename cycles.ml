let graph_of_known_cipher a b =
  let g = Graph.create() in
  for i=0 to (String.length a) - 1 do
    Graph.add_edge g (Symbol.of_char a.[i]) (Symbol.of_char b.[i]) i
  done;
  g;;

let init_paths () =
  let func li x = (Path.singleton x)::li in
  Symbol.fold func [];;

let cycles g =
  let rec rec_cycles elt_paths resu = match elt_paths with
      [] -> resu
     |p::t ->
       let s = Path.source p and finalelt = List.hd (Path.rev_path p) in
       let func (elt_paths, resu) sym =
         if sym == s
         then begin
             let path = Path.rev_path p in
             if (List.length path) == 2
             then (elt_paths, resu)
             else begin
                 let sndelt = List.hd (List.tl (List.rev path)) in
                 if Symbol.to_int sndelt < Symbol.to_int finalelt
                 then (elt_paths, (Path.snoc p s)::resu)
                 else (elt_paths, resu)
               end
           end
         else if (Path.mem p sym) || (Symbol.to_int s) > (Symbol.to_int sym)
         then (elt_paths, resu)
         else ((Path.snoc p sym)::elt_paths, resu)
       in let elt_paths,resu = Graph.fold_over_connected g func (t,resu) finalelt in
          rec_cycles elt_paths resu
  in rec_cycles (init_paths()) [];;



let () =
  if Filename.basename Sys.argv.(0) = "cycles" then begin
      let g = graph_of_known_cipher Sys.argv.(1) Sys.argv.(2) in ();
    end
