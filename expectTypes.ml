exception SyntaxError of string

module StringMap = BatMap.Make(String)
type instance = string StringMap.t
type 'a template = instance -> 'a

type matcher =
  | MatchPCRE of Pcre.regexp * string * (int * string) list
  | MatchJSON of string
  | MatchCode of (instance -> string -> instance option)
  | MatchTrivial

type matchrule = {
  request_type: Cohttp.Code.meth;
  uri: Uri.t template;
  request_body: string template option;
  result_status: Cohttp.Code.status_code option;
  result_body: matcher
}
type matchscript = matchrule list

let re_replace = Pcre.regexp ~study:true "\\${(.+?)}"

let build_template (finalizer: string -> 'a) template_string: 'a template =
  fun (instance: instance) ->
  finalizer (Pcre.substitute_substrings
               ~rex:re_replace
               ~subst:(fun key -> StringMap.find (Pcre.get_substring key 1) instance)
               template_string)

let build_body = build_template (fun s -> s)
let build_uri = build_template (Uri.of_string)
let build_pcre str bindings =
  MatchPCRE (Pcre.regexp str, str, bindings)

let matchers: (string, string -> instance -> string -> instance option) Hashtbl.t = Hashtbl.create 17

let register_matcher funcname matcher =
  if Hashtbl.mem matchers funcname then
    failwith ("Trying to register " ^ funcname ^ " twice");
  Hashtbl.add matchers funcname matcher

let build_code funcname param = MatchCode (Hashtbl.find matchers funcname param)

let build_trivial = MatchTrivial
                      
let build_json expected = MatchJSON expected
