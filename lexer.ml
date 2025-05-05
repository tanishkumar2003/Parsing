open TokenTypes
open String

(*type token =
  | Tok_RParen
  | Tok_LParen
  | Tok_Equal
  | Tok_NotEqual
  | Tok_Greater
  | Tok_Less
  | Tok_GreaterEqual
  | Tok_LessEqual
  | Tok_Or
  | Tok_And
  | Tok_Not
  | Tok_If
  | Tok_Then
  | Tok_Else
  | Tok_Add
  | Tok_Sub
  | Tok_Mult
  | Tok_Div
  | Tok_Concat
  | Tok_Let
  | Tok_Rec
  | Tok_In
  | Tok_Def
  | Tok_Fun
  | Tok_Arrow
  | Tok_Int of int
  | Tok_Bool of bool
  | Tok_String of string
  | Tok_ID of string
  | Tok_DoubleSemi*)


(* We provide the regular expressions that may be useful to your code *)

let re_rparen = Str.regexp ")";;
let re_lparen = Str.regexp "(";;
let re_equal = Str.regexp "=";;
let re_not_equal = Str.regexp "<>";;
let re_greater = Str.regexp ">";;
let re_less = Str.regexp "<";;
let re_greater_equal = Str.regexp ">=";;
let re_less_equal = Str.regexp "<=";;
let re_or = Str.regexp "||";;
let re_and = Str.regexp "&&";;
let re_not = Str.regexp "not";;
let re_if = Str.regexp "if";;
let re_then = Str.regexp "then";;
let re_else = Str.regexp "else";;
let re_add = Str.regexp "+";;
let re_sub = Str.regexp "-";;
let re_mult = Str.regexp "*";;
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^";;
let re_let = Str.regexp "let";;
let re_rec = Str.regexp "rec";;
let re_in = Str.regexp "in";;
let re_def = Str.regexp "def";;
let re_fun = Str.regexp "fun";;
let re_arrow = Str.regexp "->";;
let re_pos_int = Str.regexp "[0-9]+";;
let re_neg_int = Str.regexp "(-[0-9]+)";;
let re_true = Str.regexp "true";;
let re_false = Str.regexp "false";;
let re_string = Str.regexp "\"[^\"]*\"";;
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*";;
let re_double_semi = Str.regexp ";;";;
let re_whitespace = Str.regexp "[ \t\n]+";;

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let rec tokenize input =
  let length = String.length input in
  let rec tok pos =
    if pos >= length then
      []
    else 
      match input.[pos] with
      | ' ' | '\t' | '\n' -> tok (pos + 1)
      | '(' when pos + 2 < length && input.[pos+1] = '-' && '0' <= input.[pos+2] && input.[pos+2] <= '9' ->
          let endPos = pos + 3 in
          let rec findEnd p =
            if p >= length then p
            else if '0' <= input.[p] && input.[p] <= '9' then findEnd (p + 1)
            else p
          in
          let endNum = findEnd endPos in
          if endNum >= length || input.[endNum] <> ')' then
            raise (InvalidInputException "Invalid negative number format")
          else
            let num = String.sub input (pos + 1) (endNum - pos - 1) in
            Tok_Int(int_of_string num) :: tok (endNum + 1)
      | d when '0' <= d && d <= '9' ->
          let endPos = pos + 1 in
          let rec findEnd p =
            if p >= length then p
            else if '0' <= input.[p] && input.[p] <= '9' then findEnd (p + 1)
            else p
          in
          let endNum = findEnd endPos in
          let num = String.sub input pos (endNum - pos) in
          Tok_Int(int_of_string num) :: tok endNum
      | '(' -> Tok_LParen :: tok (pos + 1)
      | ')' -> Tok_RParen :: tok (pos + 1)
      | '"' ->
          let endPos = pos + 1 in
          let rec findEnd p =
            if p >= length then raise (InvalidInputException "Unterminated string")
            else if input.[p] = '"' then p
            else findEnd (p + 1)
          in
          let endStr = findEnd endPos in
          let str = String.sub input (pos + 1) (endStr - pos - 1) in
          Tok_String(str) :: tok (endStr + 1)
      | _ ->
          let token = match_token input pos in
          let (tkn, p) = token in
          tkn :: tok p

  and match_token input pos =
    let rest_of_input = String.sub input pos (length - pos) in
    if Str.string_match re_and rest_of_input 0 then
      (Tok_And, pos + 2)
    else if Str.string_match re_or rest_of_input 0 then
      (Tok_Or, pos + 2)
    else if Str.string_match re_not_equal rest_of_input 0 then
      (Tok_NotEqual, pos + 2)
    else if Str.string_match re_greater_equal rest_of_input 0 then
      (Tok_GreaterEqual, pos + 2)
    else if Str.string_match re_less_equal rest_of_input 0 then
      (Tok_LessEqual, pos + 2)
    else if Str.string_match re_arrow rest_of_input 0 then
      (Tok_Arrow, pos + 2)
    else if Str.string_match re_double_semi rest_of_input 0 then
      (Tok_DoubleSemi, pos + 2)
    else if Str.string_match re_true rest_of_input 0 then
      (Tok_Bool(true), pos + 4)
    else if Str.string_match re_false rest_of_input 0 then
      (Tok_Bool(false), pos + 5)
    else if Str.string_match re_if rest_of_input 0 then
      (Tok_If, pos + 2)
    else if Str.string_match re_in rest_of_input 0 then
      (Tok_In, pos + 2)
    else if Str.string_match re_then rest_of_input 0 then
      (Tok_Then, pos + 4)
    else if Str.string_match re_else rest_of_input 0 then
      (Tok_Else, pos + 4)
    else if Str.string_match re_let rest_of_input 0 then
      (Tok_Let, pos + 3)
    else if Str.string_match re_def rest_of_input 0 then
      (Tok_Def, pos + 3)
    else if Str.string_match re_rec rest_of_input 0 then
      (Tok_Rec, pos + 3)
    else if Str.string_match re_fun rest_of_input 0 then
      (Tok_Fun, pos + 3)
    else if Str.string_match re_not rest_of_input 0 then
      (Tok_Not, pos + 3)
    else if Str.string_match re_equal rest_of_input 0 then
      (Tok_Equal, pos + 1)
    else if Str.string_match re_greater rest_of_input 0 then
      (Tok_Greater, pos + 1)
    else if Str.string_match re_less rest_of_input 0 then
      (Tok_Less, pos + 1)
    else if Str.string_match re_add rest_of_input 0 then
      (Tok_Add, pos + 1)
    else if Str.string_match re_sub rest_of_input 0 then
      (Tok_Sub, pos + 1)
    else if Str.string_match re_mult rest_of_input 0 then
      (Tok_Mult, pos + 1)
    else if Str.string_match re_div rest_of_input 0 then
      (Tok_Div, pos + 1)
    else if Str.string_match re_concat rest_of_input 0 then
      (Tok_Concat, pos + 1)
    else if Str.string_match re_id rest_of_input 0 then
      let id = Str.matched_string rest_of_input in
      (Tok_ID(id), pos + String.length id)
    else
      raise (InvalidInputException "Invalid character")

  in tok 0
