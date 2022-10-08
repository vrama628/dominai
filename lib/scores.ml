type t = (string * int) list [@@deriving eq]

let yojson_of_t (scores : t) : Yojson.Safe.t =
  `Assoc (Base.List.Assoc.map ~f:(fun score -> `Int score) scores)

let t_of_yojson (json : Yojson.Safe.t) : t =
  json
  |> Yojson.Safe.Util.to_assoc
  |> Base.List.Assoc.map ~f:Yojson.Safe.Util.to_int
