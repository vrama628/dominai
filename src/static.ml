open Core

let read_file name =
  let in_channel = In_channel.create (Filename.concat "static" name) in
  let content = In_channel.input_all in_channel in
  In_channel.close in_channel;
  content

let read_lines name =
  let in_channel = In_channel.create (Filename.concat "static" name) in
  let content = In_channel.input_lines in_channel in
  In_channel.close in_channel;
  content

let docs_md = read_file "docs.md"

let adjectives = read_lines "english-adjectives.txt"

let nouns = read_lines "english-nouns.txt"

module Js = struct
  let js name =
    let in_channel =
      In_channel.create ("_build/default/src/js/" ^ name ^ ".bc.js")
    in
    let content = In_channel.input_all in_channel in
    In_channel.close in_channel;
    content

  let game_info = js "game_info"

  let player_ui = js "player_ui"
end