type 'a t = ('a, Jsonrpc.Response.Error.t) result Lwt.t

let return (x : 'a) : 'a t = Lwt.return (Ok x)

let error (fmt : ('r, unit, string, 'a t) format4) : 'r =
  Printf.ksprintf
    (fun message ->
      Lwt.return
        (Error
           Jsonrpc.Response.Error.(make ~code:Code.InvalidRequest ~message ())
        )
    )
    fmt

let ensure (condition : bool) (fmt : ('r, unit, string, unit t) format4) : 'r =
  Printf.ksprintf
    (fun message ->
      Lwt.return
        ( if condition then
          Ok ()
        else
          Error
            Jsonrpc.Response.Error.(make ~code:Code.InvalidRequest ~message ())
        )
    )
    fmt

module Let_syntax = struct
  let bind (x : 'a t) ~(f : 'a -> 'b t) : 'b t =
    match%lwt x with Ok y -> f y | Error e -> Lwt.return (Error e)
end
