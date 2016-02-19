open ExpectParser
open Kaputt
open Kaputt.Abbreviations

let escape what 
let string_of_token = function
  | STRING s -> "\"" ^ (escape "\"" s) ^ "\""
  | PATTERN p -> "/" ^ (escape "/" p) ^ "/"
  | NUMBER i -> string_of_int i
  | STATUS -> "status"
  | SET -> "set"
  | SEMICOLON -> ";"
  | RBRACE -> "}"
  | PUT -> "PUT"
  | POST -> "POST"
  | OPTIONS -> "OPTIONS"
  | MATCH -> "match"
  | LBRACE -> "{"
  | HEAD -> "HEAD"
  | GET -> "GET"
  | EQUALS -> "="
  | EOF -> ""
  | DELETE -> "DELETE"

let gen_token =
  let open Generator in
    choose_list [
      lift STATUS "status";
      lift SET "set";
      lift SEMICOLON ";";
      lift RBRACE "{";
      lift LBRACE "}";
      lift PUT "PUT";
      lift POST "POST";
      lift OPTIONS "OPTIONS";
      lift MATCH "match";
      lift HEAD "HEAD";
      lift GET "GET";
      lift EQUALS "=";
      lift DELETE "DELETE";
      map1 (fun s -> STRING s) string_of_token (string pos_int alphanum);
      map1 (fun s -> PATTERN s) string_of_token (string pos_int alphanum);
      map1 (fun i -> NUMBER i) string_of_token pos_int
    ]

let gen_pattern_with_space =
  let open Generator in
  list pos_int (zip2 gen_token pos_int)


       (*
let test_keywords =

let int = ['0'-'9']+
let identifier = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let white = [' ' '\t' '\r' '\n']+

rule char = parse
    | _ { Lexing.lexeme lexbuf }

and pattern buf = parse
    | "\\" { Buffer.add_string buf (char lexbuf); pattern buf lexbuf }
    | "/" { Buffer.contents buf }
    | _ { Buffer.add_string buf (Lexing.lexeme lexbuf); pattern buf lexbuf }

and string buf = parse
    | "\\" { Buffer.add_string buf (char lexbuf); pattern buf lexbuf }
    | "\"" { Buffer.contents buf }
    | _ { Buffer.add_string buf (Lexing.lexeme lexbuf); pattern buf lexbuf }

and main = parse
    | white { main lexbuf }
    | "GET" { GET }
    | "HEAD" { HEAD }
    | "POST" { POST }
    | "PUT" { PUT }
    | "DELETE" { DELETE }
    | "OPTIONS" { OPTIONS }
    | "STATUS" { STATUS }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | ";" { SEMICOLON }
    | "SET" { SET }
    | "MATCH" { MATCH }
    | "=" { EQUALS }
    | int { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
    | "/" { PATTERN (pattern (Buffer.create 23) lexbuf) }
    | identifier { STRING (Lexing.lexeme lexbuf) }
    | "\"" { STRING (string (Buffer.create 23) lexbuf) }
        *)
