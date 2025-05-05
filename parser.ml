open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) =
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) =
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
  match lookahead toks with
  | Some Tok_Let -> parse_let toks
  | Some Tok_If -> parse_if toks 
  | Some Tok_Fun -> parse_fun toks
  | _ -> parse_or toks

and parse_let toks =
  let t = match_token toks Tok_Let in
  let is_rec = match lookahead t with
    | Some Tok_Rec -> 
        let t2 = match_token t Tok_Rec in
        (true, t2)
    | _ -> (false, t)
  in
  match lookahead (snd is_rec) with
  | Some (Tok_ID id) ->
      let t2 = match_token (snd is_rec) (Tok_ID id) in
      let t3 = match_token t2 Tok_Equal in
      let (t4, e1) = parse_expr t3 in
      let t5 = match_token t4 Tok_In in
      let (t6, e2) = parse_expr t5 in
      (t6, Let(id, fst is_rec, e1, e2))
  | _ -> raise (InvalidInputException "Expected identifier after let")

and parse_fun toks =
  let t = match_token toks Tok_Fun in
  match lookahead t with
  | Some (Tok_ID id) ->
      let t2 = match_token t (Tok_ID id) in
      let t3 = match_token t2 Tok_Arrow in
      let (t4, e) = parse_expr t3 in
      (t4, Fun(id, e))
  | _ -> raise (InvalidInputException "Expected identifier after fun")

and parse_if toks =
  let t = match_token toks Tok_If in
  let (t2, e1) = parse_expr t in
  let t3 = match_token t2 Tok_Then in
  let (t4, e2) = parse_expr t3 in
  let t5 = match_token t4 Tok_Else in
  let (t6, e3) = parse_expr t5 in
  (t6, If(e1, e2, e3))

and parse_or toks =
  let (t1, e1) = parse_and toks in
  match lookahead t1 with
  | Some Tok_Or ->
      let t2 = match_token t1 Tok_Or in
      let (t3, e2) = parse_or t2 in
      (t3, Binop(Or, e1, e2))
  | _ -> (t1, e1)

and parse_and toks =
  let (t1, e1) = parse_equality toks in
  match lookahead t1 with
  | Some Tok_And ->
      let t2 = match_token t1 Tok_And in
      let (t3, e2) = parse_and t2 in
      (t3, Binop(And, e1, e2))
  | _ -> (t1, e1)

and parse_equality toks =
  let (t1, e1) = parse_relational toks in
  match lookahead t1 with
  | Some Tok_Equal ->
      let t2 = match_token t1 Tok_Equal in
      let (t3, e2) = parse_equality t2 in
      (t3, Binop(Equal, e1, e2))
  | Some Tok_NotEqual ->
      let t2 = match_token t1 Tok_NotEqual in
      let (t3, e2) = parse_equality t2 in
      (t3, Binop(NotEqual, e1, e2))
  | _ -> (t1, e1)

and parse_relational toks =
  let (t1, e1) = parse_additive toks in
  match lookahead t1 with
  | Some Tok_Greater ->
      let t2 = match_token t1 Tok_Greater in
      let (t3, e2) = parse_relational t2 in
      (t3, Binop(Greater, e1, e2))
  | Some Tok_Less ->
      let t2 = match_token t1 Tok_Less in
      let (t3, e2) = parse_relational t2 in
      (t3, Binop(Less, e1, e2))
  | Some Tok_GreaterEqual ->
      let t2 = match_token t1 Tok_GreaterEqual in
      let (t3, e2) = parse_relational t2 in
      (t3, Binop(GreaterEqual, e1, e2))
  | Some Tok_LessEqual ->
      let t2 = match_token t1 Tok_LessEqual in
      let (t3, e2) = parse_relational t2 in
      (t3, Binop(LessEqual, e1, e2))
  | _ -> (t1, e1)

and parse_additive toks =
  let (t1, e1) = parse_multiplicative toks in
  match lookahead t1 with
  | Some Tok_Add ->
      let t2 = match_token t1 Tok_Add in
      let (t3, e2) = parse_additive t2 in
      (t3, Binop(Add, e1, e2))
  | Some Tok_Sub ->
      let t2 = match_token t1 Tok_Sub in
      let (t3, e2) = parse_additive t2 in
      (t3, Binop(Sub, e1, e2))
  | _ -> (t1, e1)

and parse_multiplicative toks =
  let (t1, e1) = parse_concat toks in
  match lookahead t1 with
  | Some Tok_Mult ->
      let t2 = match_token t1 Tok_Mult in
      let (t3, e2) = parse_multiplicative t2 in
      (t3, Binop(Mult, e1, e2))
  | Some Tok_Div ->
      let t2 = match_token t1 Tok_Div in
      let (t3, e2) = parse_multiplicative t2 in
      (t3, Binop(Div, e1, e2))
  | _ -> (t1, e1)

and parse_concat toks =
  let (t1, e1) = parse_unary toks in
  match lookahead t1 with
  | Some Tok_Concat ->
      let t2 = match_token t1 Tok_Concat in
      let (t3, e2) = parse_concat t2 in
      (t3, Binop(Concat, e1, e2))
  | _ -> (t1, e1)

and parse_unary toks =
  match lookahead toks with
  | Some Tok_Not ->
      let t1 = match_token toks Tok_Not in
      let (t2, e) = parse_unary t1 in
      (t2, Not(e))
  | _ -> parse_function_call toks

and parse_function_call toks =
  let (t1, e1) = parse_primary toks in
  match lookahead t1 with
  | Some Tok_Int _ | Some Tok_Bool _ | Some Tok_String _ | Some Tok_ID _ | Some Tok_LParen ->
      let (t2, e2) = parse_primary t1 in
      (t2, FunctionCall(e1, e2))
  | _ -> (t1, e1)

and parse_primary toks =
  match lookahead toks with
  | Some (Tok_Int i) ->
      (match_token toks (Tok_Int i), Int i)
  | Some (Tok_Bool b) ->
      (match_token toks (Tok_Bool b), Bool b)
  | Some (Tok_String s) ->
      (match_token toks (Tok_String s), String s)
  | Some (Tok_ID id) ->
      (match_token toks (Tok_ID id), ID id)
  | Some Tok_LParen ->
      let t1 = match_token toks Tok_LParen in
      let (t2, e) = parse_expr t1 in
      let t3 = match_token t2 Tok_RParen in
      (t3, e)
  | _ -> raise (InvalidInputException "Expected an expression")
