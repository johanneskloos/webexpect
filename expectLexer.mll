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

