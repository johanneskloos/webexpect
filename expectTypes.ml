type matchrule = {
  request_type: Cohttp.Code.meth;
  uri: (string * string) list -> Uri.t;
  request_body: ((string * string) list -> string) option;
  result_status: Cohttp.Code.status_code option;
  result_body_pattern: Pcre.regexp;
  result_body_pattern_string: string;
  result_binding: (int * string) list
}
type matchscript = matchrule list

let build_template tmpl inst =
  tmpl (* TODO *)

let build_body = build_template
let build_uri tmpl inst = Uri.of_string (build_template tmpl inst)
                            
