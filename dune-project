(lang dune 3.0)
(generate_opam_files true)
(package
  (name dominai)
  (synopsis "A web server for running dominion games")
  (depends
    core
    dream
    jsonrpc
    yojson
    react
    ppx_yojson_conv
    lwt_ppx
    ppx_sexp_conv
    stdio
    ppx_deriving
    tyxml
    omd
  )
)
