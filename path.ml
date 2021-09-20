type t = Symbol.sym * (Symbol.sym list);;

let source (s,_)= s;;

let rev_path (_,p) = p ;;

let mem (_,p) x = List.mem x p;;

let singleton s = (s,[s]);;

let snoc (s,p) x = (s,x::p);;

let rec compare (s,p) (s',p') = match p,p' with
    [],[] -> 0
   |[],(_::_) -> 1
   |_,[] -> -1
   |(h::t),(h'::t') when h==h' -> compare (s,t) (s',t')
   |(h::_),(h'::_) when (Symbol.to_int h) < (Symbol.to_int h') -> -1
   |_,_ -> 1;;
