%{
    open ExpectTypes
%}
%token <int> NUMBER
%token <string> PATTERN STRING
%token GET PUT POST HEAD DELETE OPTIONS STATUS SEMICOLON
%token LBRACE RBRACE SET MATCH EQUALS COMMA
%token EOF LOAD WITH JSON
%start <ExpectTypes.instance * ExpectTypes.matchscript> prog
%type <ExpectTypes.instance -> ExpectTypes.instance> directive
%%
prog:
    | init=directives r=rules EOF { (init, r) };

directives:
    | { StringMap.empty };
    | init=directives upd=directive SEMICOLON { upd init };

directive:
    | LOAD name=STRING
    {
        try
            Dynlink.loadfile (Dynlink.adapt_filename (name ^ ".cmx")); fun init -> init
        with Dynlink.Error e ->
            failwith (Dynlink.error_message e)
    }
    | SET name=STRING EQUALS value=STRING { StringMap.add name value };

rules:
    | r=rule { [r] }
    | rs=rules r=rule { r::rs };

rule:
    | request_type=httpmethod uri=STRING request_body=body SEMICOLON
    { { request_type; uri = build_uri uri; request_body;
        result_status = None; result_body = build_trivial } }
    | request_type=httpmethod uri=STRING request_body=body 
      LBRACE m=result_matches RBRACE SEMICOLON?
    { let (result_status, result_body) = m in
        { request_type; uri = build_uri uri; request_body; result_status; result_body } };
    
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
    | STATUS n=NUMBER SEMICOLON?
      { (Some (Cohttp.Code.status_of_code n), build_trivial) }
    | STATUS n=NUMBER SEMICOLON b=body_match SEMICOLON?
      { (Some (Cohttp.Code.status_of_code n), b) }
    | b=body_match SEMICOLON?
      { (None, b) };

body_match:
    | MATCH p=PATTERN
    { build_pcre p [] }
    | MATCH p=PATTERN WITH b=separated_list(COMMA, set_expr)
    { build_pcre p b }
    | MATCH JSON str=STRING
    { build_json str }
    | MATCH WITH func=STRING param=STRING
    { build_code func param };
      

set_expr:
    | n=STRING EQUALS i=NUMBER { (i, n) };
