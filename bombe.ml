
let rec step_plus_n config n = match n with
    0 -> config
   |n -> step_plus_n (Enigma.step config) (n-1)

let constraint_from_cycle cycle g config =
  let try_with_sym li a_start =
    let rec aux_rec c a results = match c with
        [] -> results
       |[_] -> results
       |s1::(s2::t) -> let constraint_with_this_m m results =
                         let config2 = step_plus_n config m in
                         let b = Enigma.image config2 a in
                         match t with
                           [] -> if b <> a_start
                                 then (a_start, s2)::results
                                 else results
                          |_ -> aux_rec (s2::t) b results;
                       in let machines = Graph.get_edge g s1 s2
                          in Graph.Positions.fold constraint_with_this_m machines results
                            
    in aux_rec cycle a_start li
  in Symbol.fold try_with_sym [];;

let infer_plug_constraint cycles g config =
  let rec infer_plug_constraint_rec cycles = match cycles with
    [] -> []
   |c::t -> let constraints = infer_plug_constraint_rec t in
            (constraint_from_cycle (Path.rev_path c) g config)@constraints;
  in infer_plug_constraint_rec (Cycles.to_paths cycles);;

let rec removes_all_constraints p lc = match lc with
    [] -> ()
   |(a,b)::t -> (Board.remove_assoc p a b; removes_all_constraints p t;);;

let () =
  if Filename.basename Sys.argv.(0) = "bombe" then begin
      let g = Cycles.graph_of_known_cipher Sys.argv.(1) Sys.argv.(2) in
      let config = Enigma.test and cycles = Cycles.cycles g
      in let constraints = infer_plug_constraint cycles g config
         in let b = Board.top()
            in removes_all_constraints b constraints;
               Board.print_board b;
    end;
