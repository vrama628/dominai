type t = (string * int) list

let yojson_of_t (scores : t) : Yojson.Safe.t =
  `Assoc (Base.List.Assoc.map ~f:(fun score -> `Int score) scores)
