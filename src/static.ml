open Core

let read_file name =
  let in_channel = In_channel.create (Filename.concat "static" name) in
  let content = In_channel.input_all in_channel in
  In_channel.close in_channel;
  content

let docs_md = read_file "docs.md"