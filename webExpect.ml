open ExpectTypes

let parse_script file =
  let chan = open_in file in
  let ast = ExpectParser.prog ExpectLexer.main (Lexing.from_channel chan) in
    close_in chan; ast

exception Mismatch of matchrule * string * string * (string * string) list

let interpret_match_rule rule bindings =
  let open Cohttp.Code in
  let open Cohttp_lwt_unix.Client in
  let { request_type; uri; request_body; result_status; result_body_pattern;
        result_binding } = rule in
  let%lwt (response, body)  =
    call
      ?body:(BatOption.map (fun f -> Cohttp_lwt_body.of_string (f bindings)) request_body)
      request_type (uri bindings)
  in begin match result_status with
    | Some expectation ->
        let actual = Cohttp_lwt_unix.Response.status response in
        if actual <> expectation then
          raise (Mismatch(rule, "response", string_of_status actual, bindings))
    | None -> ()
  end;
     let%lwt body_text = Cohttp_lwt_body.to_string body in
       try
         let sub = Pcre.exec ~rex:result_body_pattern body_text in
         let new_bindings =
           List.map (fun (idx, name) -> (name, Pcre.get_substring sub idx)) result_binding
         in Lwt.return (new_bindings @ bindings)
       with Not_found ->
         raise (Mismatch(rule, "body", body_text, bindings))

let interpret_match_rules =
  Lwt_list.fold_right_s interpret_match_rule

let pp_matchrule bindings pp
      { request_type; uri; request_body; result_status; result_body_pattern_string;
        result_binding } =
  Format.fprintf pp "@[<hov 2>%s %s@ with@ %a@, expecting %a@ and@ %s@ and binding@ %a@]"
    (Cohttp.Code.string_of_method request_type)
    (Uri.to_string (uri bindings))
    (Fmt.option Fmt.string) (BatOption.map (fun f -> f bindings) request_body)
    (Fmt.option (fun pp rs -> Fmt.string pp (Cohttp.Code.string_of_status rs))) result_status
    result_body_pattern_string
    (Fmt.list (Fmt.pair Fmt.int Fmt.string)) result_binding

let num_failed = ref 0

let run_script file =
  try 
    ignore (Lwt_main.run (interpret_match_rules (parse_script file) []))
  with Mismatch (rule, what, value, bindings) ->
    incr num_failed;
    Format.eprintf
      "@[<v 2>Test script %s failed: %s was [@<hov 2>%s@] for match rule@ %a@ and bindings
      @[<hov 2>%a@]@."
      file what value (pp_matchrule bindings) rule
      (Fmt.list (Fmt.pair Fmt.string Fmt.string)) bindings

let _ =
  BatEnum.iter run_script (BatPervasives.args ());
  if !num_failed > 0 then begin
    Format.eprintf "%d tests failed@." !num_failed;
    exit 1
  end else
    exit 0


