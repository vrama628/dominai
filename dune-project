(lang dune 3.0)
(generate_opam_files true)
(package
  (name dominai)
  (synopsis "A web server for running dominion games")
  (depends
    base
    dream
    jsonrpc
    yojson
    react
    ppx_deriving_yojson    
  )
)
