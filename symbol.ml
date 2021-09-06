
type sym = int;;

let nb_syms = 26;;

let (a:sym) = 0;;

let letters = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z'];;

let (symbols:sym list) = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25];;

let of_int (integer:int) = (integer:sym);;

let to_int (symbol:sym) = (symbol:int);;

let to_char (symbol:sym) =
  let rec r_to_char i letters = match letters with
    [] -> failwith "Wrong Symbol !"
    |h::t -> if i == 0 then h else r_to_char (i-1) t
  in r_to_char (to_int symbol) letters;;

let of_char (character:char) =
  let rec r_of_char i letters = match letters with
      [] -> failwith "No such character"
     |h::_ when h == character -> i
     |_::t -> r_of_char (i+1) t
  in r_of_char 0 letters;;

let next symb = of_int ((to_int symb + 1) mod nb_syms);;

let (++) symb1 symb2 = of_int (((to_int symb1) + to_int symb2) mod nb_syms);;

let (--) symb1 symb2 = of_int (((to_int symb1) - to_int symb2 + nb_syms) mod nb_syms);;

let iter f =
  let rec r_iter symbols = match symbols with
    [] -> ()
    |h::t -> begin
        ignore (f h);
        r_iter t;
      end;
  in r_iter symbols;;

let fold f x =
  let rec r_fold symbols_rev = match symbols_rev with
      [] -> x
     |h::t -> f (r_fold t) h
  in r_fold (List.rev symbols);;


let print_sym (symb:sym) =
  Printf.fprintf "%c" (to_char symb);;

module Set =
  struct
    
    type t = int;;

    let empty = 0;;

    let singleton symb = Int.shift_left 1 symb;;
    
    let member symb s =
      (Int.logand s (Int.shift_left 1 symb)) <> 0;;

    let add symb s =
      Int.logor s (Int.shift_left 1 symb);;

  end

module Map =
  struct
    type 'a t = 'a array;;
    
    let get = Array.get;;

    let set = Array.set;;

    let make (x:'a) = Array.make nb_syms x;;

    let init (f:sym -> 'a) = ((Array.init nb_syms f):'a t);;

    let copy = Array.copy;;

    let map = Array.map;;

    let inverse map =
      let map2 = make a in
      for symb=0 to nb_syms-1 do
        map2.(map.(symb)) <- symb
      done;
      map2;;

    let print_tmap out_channel map =
      for i=0 to nb_syms-1 do
        Printf.fprintf out_channel "%c" (to_char map.(i));
      done;
      ;;

  end
