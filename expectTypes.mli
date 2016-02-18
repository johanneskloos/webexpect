module StringMap : BatMap.S with type key = string
type instance = string StringMap.t
type 'a template = instance -> 'a
type matchrule = {
  request_type : Cohttp.Code.meth;
  uri : Uri.t template;
  request_body : string template option;
  result_status : Cohttp.Code.status_code option;
  result_body_pattern : Pcre.regexp;
  result_body_pattern_string : string;
  result_binding : (int * string) list;
}
type matchscript = matchrule list

val re_any : Pcre.regexp
val pattern_any : string

val build_body : string -> string template
val build_uri : string -> Uri.t template
