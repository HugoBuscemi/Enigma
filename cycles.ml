let graph_of_known_cipher a b =
  let g = Graph.create() in
  for i=0 to (String.length a) - 1 do
    Graph.add_edge g (Symbol.of_char a.[i]) (Symbol.of_char b.[i]) i
  done;
  g;;


let () =
  if Filename.basename Sys.argv.(0) = "tests" then begin
      let g = graph_of_known_cipher Sys.argv.(1) Sys.argv.(2) in ();
    end
