module StringMap = BatMap.Make(String)
type instance = string StringMap.t
type 'a template = instance -> 'a

type matchrule = {
  request_type: Cohttp.Code.meth;
  uri: Uri.t template;
  request_body: string template option;
  result_status: Cohttp.Code.status_code option;
  result_body_pattern: Pcre.regexp;
  result_body_pattern_string: string;
  result_binding: (int * string) list
}
type matchscript = matchrule list

let re_replace = Pcre.regexp ~study:true "\\${(.+?)}"
let re_any = Pcre.regexp ""
let pattern_any = ""

let build_template (finalizer: string -> 'a) template_string: 'a template =
  fun (instance: instance) ->
  finalizer (Pcre.substitute
               ~rex:re_replace
               ~subst:(fun key -> StringMap.find key instance)
               template_string)

let build_body = build_template (fun s -> s)
let build_uri = build_template (Uri.of_string)
                            
