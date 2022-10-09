open Core
include Dominai_client_lib

module List = struct
  include List

  let diff l1 l2 =
    let rec remove_elt l v =
      match l with
      | [] -> []
      | e :: ls -> if Card.equal e v then ls else e :: remove_elt ls v
    in
    List.fold l2 ~init:l1 ~f:remove_elt
end
