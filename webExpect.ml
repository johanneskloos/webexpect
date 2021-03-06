open ExpectTypes

let pp_position pp lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Format.fprintf pp "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let string_of_token = let open ExpectParser in function
  | STRING s -> "\"" ^ s ^ "\""
  | PATTERN p -> "/" ^ p ^ "/"
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
  | WITH -> "with"
  | LOAD -> "load"
  | JSON -> "json"
  | COMMA -> ","

let parse_script file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan
  and chatty_lexer lexbuf =
    let token = ExpectLexer.main lexbuf in
      (*Format.eprintf "Got token %s@." (string_of_token token);*)
      token
  in try
      let ast = ExpectParser.prog chatty_lexer lexbuf
      in close_in chan; ast
    with
      | ExpectParser.Error ->
          Format.eprintf "Syntax error at %a@." pp_position lexbuf;
          close_in chan;
          raise Exit


exception Mismatch of matchrule * string * string * instance

let interpret_matcher rule matcher bindings body_text = match matcher with
  | MatchPCRE (rex, rex_string, bind_pattern) ->
      begin try
        let sub = Pcre.exec ~rex body_text in
          List.fold_left (fun bindings (index, name) ->
                            let value = Pcre.get_substring sub index in
                              StringMap.add name value bindings)
            bindings bind_pattern
      with Not_found ->
        raise (Mismatch(rule, "body pcre match", body_text, bindings))
      end
  | MatchJSON expected ->
      begin try
        let open Yojson.Basic in
          Format.eprintf "Exp template %s@." expected;
        let exp_json = ExpectTypes.build_template (fun s -> s) expected bindings in
          Format.eprintf "Instantiating exp template gives %s@." exp_json;
          Format.eprintf "Body %s@." body_text;
        let got_body = from_string body_text
        and exp_body = ExpectTypes.build_template from_string expected bindings
        in if exp_body = got_body then
          bindings
        else
          raise (Mismatch(rule, "body JSON match", body_text, bindings))
      with _ -> (* parse error *)
        raise (Mismatch(rule, "body JSON match (parse error) ", body_text, bindings))
      end
  | MatchCode mfunc ->
      begin match mfunc bindings body_text with
        | Some new_bindings -> new_bindings
        | None ->
            raise (Mismatch(rule, "body function match", body_text, bindings))
      end
  | MatchTrivial -> bindings

let interpret_match_rule rule bindings =
  let open Cohttp.Code in
  let open Cohttp_lwt_unix.Client in
  let open Lwt in
  let { request_type; uri; request_body; result_status; result_body } = rule in
    Format.printf "Sending %s request to %s@."
      (string_of_method request_type) (Uri.to_string (uri bindings));
  let%lwt (response, body)  =
    call
      ?body:(BatOption.map (fun f -> Cohttp_lwt_body.of_string (f bindings)) request_body)
      request_type (uri bindings)
  in
    Format.printf "Received answer with status code %s@."
      (string_of_status (Cohttp_lwt_unix.Response.status response));
    begin match result_status with
      | Some expectation ->
          let actual = Cohttp_lwt_unix.Response.status response in
            if actual <> expectation then
              raise (Mismatch(rule, "response", string_of_status actual, bindings))
      | None -> ()
    end;
    Cohttp_lwt_body.to_string body >|=
      interpret_matcher rule result_body bindings

let interpret_match_rules =
  Lwt_list.fold_right_s interpret_match_rule

let pp_matcher pp = function
  | MatchPCRE (_, re, []) ->
      Format.fprintf pp "PCRE %s" re
  | MatchPCRE (_, re, bindings) ->
      Format.fprintf pp "PCRE %s and binds [@[<hov 2>%a@]]" re
        (Fmt.list (Fmt.pair ~sep:(Fmt.const Fmt.string "=") Fmt.int Fmt.string))
        bindings
  | MatchJSON expected ->
      Format.fprintf pp "JSON equivalent to %s" expected
  | MatchCode _ ->
      Format.fprintf pp "some function"
  | MatchTrivial ->
      Format.fprintf pp "everything"

let pp_matchrule bindings pp
      { request_type; uri; request_body; result_status; result_body } =
  Format.fprintf pp "@[<hov 2>%s %s@ with@ %a@, expecting %a status@, body matches@ %a@]"
    (Cohttp.Code.string_of_method request_type)
    (Uri.to_string (uri bindings))
    (Fmt.option Fmt.string) (BatOption.map (fun f -> f bindings) request_body)
    (Fmt.option ~none:(Fmt.const Fmt.string "any")
       (fun pp rs -> Fmt.string pp (Cohttp.Code.string_of_status rs))) result_status
    pp_matcher result_body

let num_failed = ref 0

let run_script file =
  try 
    let (init, script) = parse_script file in
    ignore (Lwt_main.run (interpret_match_rules script init))
  with Mismatch (rule, what, value, bindings) ->
    incr num_failed;
    Format.eprintf
      "@[<v 2>Test script %s failed: %s was @[<hov 2>%s@] for match rule@ %a@ with bindings {@[<hov 2>%a@]}@."
      file what value (pp_matchrule bindings) rule
      (Fmt.using StringMap.bindings (Fmt.list (Fmt.pair ~sep:(Fmt.const Fmt.string "=") Fmt.string Fmt.string))) bindings
    | Exit -> (* error has already been handled *) ()

let _ =
  BatEnum.iter run_script (BatPervasives.args ());
  if !num_failed > 0 then begin
    Format.eprintf "%d tests failed@." !num_failed;
    exit 1
  end else
    exit 0


