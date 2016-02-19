open Cohttp.Code
open Cohttp_lwt_unix.Client
open Cohttp.Response
open Lwt

let send_file ~uri ~fname =
  Lwt_io.with_file ~mode:Lwt_io.Input fname
    (fun chan ->
       let%lwt (resp, body) = post
         ~body:(Cohttp_lwt_body.of_stream (Lwt_io.read_lines chan))
         (Uri.of_string uri)
       in let%lwt str_body = Cohttp_lwt_body.to_string body
       in
         Format.printf "@[<v>Result: %s@ @ Body:@ %s@]@."
           (string_of_status resp.status)
           str_body;
         Lwt.return_unit)

let () =
  Lwt_main.run (send_file ~uri:Sys.argv.(1) ~fname:Sys.argv.(2))
