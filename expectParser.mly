%{
    open ExpectTypes
%}
%token <int> NUMBER
%token <string> PATTERN STRING
%token GET PUT POST HEAD DELETE OPTIONS STATUS SEMICOLON
%token LBRACE RBRACE SET MATCH EQUALS
%token EOF
%start <ExpectTypes.matchscript> prog
%%
prog:
    | r=rev_rules EOF { List.rev r };

rev_rules:
    | r=rule { [r] }
    | rs=rev_rules r=rule { r::rs };

rule:
    | request_type=httpmethod uri=STRING request_body=body SEMICOLON
    { { request_type; uri = build_uri uri; request_body;
        result_status = None; result_body_pattern = re_any; result_binding = [];
        result_body_pattern_string = pattern_any } }
    | request_type=httpmethod uri=STRING request_body=body 
      LBRACE m=result_matches RBRACE
    { let (result_status, result_body_pattern, result_body_pattern_string, result_binding) = m in
        { request_type; uri = build_uri uri; request_body;
          result_status; result_body_pattern; result_body_pattern_string; result_binding} };
    
httpmethod:
    | GET { let open Cohttp.Code in `GET }
    | PUT { let open Cohttp.Code in `PUT }
    | POST { let open Cohttp.Code in `POST }
    | HEAD { let open Cohttp.Code in `HEAD }
    | DELETE { let open Cohttp.Code in `DELETE }
    | OPTIONS { let open Cohttp.Code in `OPTIONS };

body:
    | t=STRING { Some (build_body t) }
    | { None };

result_matches:
    | STATUS n=NUMBER { (Some (Cohttp.Code.status_of_code n), re_any, pattern_any, []) }
    | STATUS n=NUMBER SEMICOLON b=body_match
      { let (bodypat, bodypat', bindings) = b in
        (Some (Cohttp.Code.status_of_code n), bodypat, bodypat', bindings) }
    | b=body_match
      { let (bodypat, bodypat', bindings) = b in
        (None, bodypat, bodypat', bindings) };

body_match:
    | MATCH p=PATTERN { (Pcre.regexp p, p, []) }
    | MATCH p=PATTERN SEMICOLON b=separated_list(SEMICOLON, set_expr)
      { (Pcre.regexp p, p, b) };

set_expr:
    | SET n=STRING EQUALS i=NUMBER { (i, n) };
