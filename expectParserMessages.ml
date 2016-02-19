
(* This file was auto-generated based on "expectParser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Expect a HTTP method at the start of a rule.\n"
    | 14 ->
        "Expect a semicolon or a matching block after a HTTP request.\n"
    | 12 ->
        "Expect a message body or a matching block.\n"
    | 8 ->
        "Expect a HTTP method at the start of a rule.\n"
    | 16 ->
        "Expect a status match or a body pattern match inside a matching block.\n"
    | 17 ->
        "Expect a return code, given as a number, after 'status'.\n"
    | 18 ->
        "Expect a closing brace or a body pattern match after a status match.\n"
    | 19 ->
        "Expect a body pattern match or a closing brace after a status match.\n"
    | 20 ->
        "Expect a regular expression pattern after match.\n"
    | 21 ->
        "Expect a semicolon or a closing brace after a match pattern.\n"
    | 22 ->
        "Expect a set expression or a closing brace after a match pattern.\n"
    | 24 ->
        "Expecting an '=' sign between variable name and pattern number.\n"
    | 25 ->
        "Expecting a number, referencing a capturing pattern, at the RHS of a set expression.\n"
    | 27 ->
        "Expecting a semicolon or closing brace at the end of a set expression.\n"
    | 28 ->
        "Expecting a valid set expression, which should start with \"set\".\n"
    | 23 ->
        "Expecting a valid variable name on the LHS of set.\n"
    | 11 ->
        "Expecting an URL after the HTTP method.\n"
    | _ ->
        raise Not_found
