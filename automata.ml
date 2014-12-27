type regex =
    | Vide
    | Epsilon
    | Letter of char
    | Star of regex
    | Or of regex * regex
    | Conc of regex * regex ;;

type automata = {exit : bool list; trans : (int * int * char) list};;

let rec pretty r = match r with
    | Vide -> "Vide"
    | Epsilon -> "Epsilon"
    | Letter l -> String.make 1 l 
    | Star r -> (match r with
        | Conc _ -> "(" ^ (pretty r) ^ ")*"
        | _ -> (pretty r) ^ "*")
    | Or (a,b) -> "(" ^ (pretty a) ^ "+" ^ (pretty b) ^ ")"
    | Conc (a,b) -> (pretty a) ^ (pretty b);;

let rec divide lang c = match lang with
    | Vide -> Vide
    | Epsilon -> Vide
    | Letter l when (l = c) -> Epsilon 
    | Letter _ -> Vide
    | Star a -> (match divide a c with
        | Vide -> Vide
        | Epsilon -> Star a
        | la -> Conc(la, Star a))
    | Or (a,b) -> (match (divide a c, divide b c) with
        | (Vide, lb) -> lb
        | (la, Vide) -> la
        | (la, lb) -> Or (la,lb))
    | Conc (a,b) -> (match a with
        | Epsilon -> divide b c
        | a -> (match divide a c with
            | Vide -> Vide
            | Epsilon -> b
            | la -> Conc (la,b)));;

let rec epsilon_in lang = match lang with
    | Vide -> false
    | Epsilon -> true
    | Letter _ -> false
    | Star _ -> true
    | Or (a,b) -> (epsilon_in a) || (epsilon_in b)
    | Conc (a,b) -> (epsilon_in a) && (epsilon_in b);;
    

let create_aut lang alpha = 
    let langs = Hashtbl.create 44 in
        let rec calc_divs a trans lang nb todo exit = match a with
            | [] -> (trans, todo, nb, exit)
            | x::s -> begin
                let r = divide lang x in 
                    if (Hashtbl.mem langs r) then
                        calc_divs s (((Hashtbl.find langs lang),(Hashtbl.find langs r), x)::trans) lang nb todo exit
                    else begin
                        Hashtbl.add langs r nb;
                        let e = epsilon_in r in calc_divs s (((Hashtbl.find langs lang), nb, x)::trans) lang (nb+1) (r::todo) (e::exit)
                    end;
                end;
        in
            let rec iterate_todo a trans todo nb exit = match todo with
                | [] -> {exit = List.rev exit ; trans = trans}
                | x::s -> let (tr, t, nb, e) = calc_divs a trans x nb [] exit in iterate_todo a tr (List.rev_append s t) nb e
            in
                Hashtbl.add langs lang 0;
                let e = epsilon_in lang in iterate_todo alpha [] [lang] 1 [e];;

let  pretty_aut aut = 
    let rec p_a i a = match a with
        | {exit = [] ; trans = []} -> "" 
        | {exit = l ; trans = (a,b,c)::s} -> (string_of_int a) ^ " " ^ (String.make 1 c) ^ "---> " ^ (string_of_int b) ^ "\n" ^ (p_a i {exit = l ; trans = s})
        | {exit = x::s ; trans = []} when x -> (string_of_int i) ^ " " ^ "---> []" ^ "\n" ^ (p_a (i+1) {exit = s ; trans = []})
        | {exit = x::s ; trans = []} -> p_a (i+1) {exit = s ; trans = []}
    in p_a 0 aut;;


let r = Conc(Letter 'a',Star(Or(Conc(Letter 'b', Letter 'c'), Letter 'a')));;

print_endline (pretty r);;
print_endline (pretty (divide r 'a'));;
print_endline (pretty (divide (divide r 'a') 'b'));;
print_endline (pretty_aut (create_aut r ['a'; 'b'; 'c']));;

