let graph_of_known_cipher a b =
  let g = Graph.create() in
  for i=0 to (String.length a) - 1 do
    Graph.add_edge g (Symbol.of_char a.[i]) (Symbol.of_char b.[i]) i
  done;
  g;;
