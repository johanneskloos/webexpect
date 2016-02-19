{
    open ExpectParser
}
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
    | "\\" { Buffer.add_string buf (char lexbuf); string buf lexbuf }
    | "\"" { Buffer.contents buf }
    | _ { Buffer.add_string buf (Lexing.lexeme lexbuf); string buf lexbuf }

and string' buf = parse
    | "\\" { Buffer.add_string buf (char lexbuf); string' buf lexbuf }
    | "'" { Buffer.contents buf }
    | _ { Buffer.add_string buf (Lexing.lexeme lexbuf); string' buf lexbuf }

and main = parse
    | white { main lexbuf }
    | "GET" { GET }
    | "HEAD" { HEAD }
    | "POST" { POST }
    | "PUT" { PUT }
    | "DELETE" { DELETE }
    | "OPTIONS" { OPTIONS }
    | "status" { STATUS }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | ";" { SEMICOLON }
    | "set" { SET }
    | "match" { MATCH }
    | "=" { EQUALS }
    | "load" { LOAD }
    | "with" { WITH }
    | "," { COMMA }
    | "json" { JSON }
    | int { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
    | "\"" { STRING (string (Buffer.create 23) lexbuf) }
    | "'" { STRING (string' (Buffer.create 23) lexbuf) }
    | "/" { PATTERN (pattern (Buffer.create 23) lexbuf) }
    | identifier { STRING (Lexing.lexeme lexbuf) }
    | _ { raise (ExpectTypes.SyntaxError ("Unknown character: " ^ Lexing.lexeme lexbuf)) }
    | eof { EOF }
