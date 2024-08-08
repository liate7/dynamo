open! ContainersLabels
open! Eio.Std

let main stdin stdout = Dynamo.repl stdin stdout

let () =
  Eio_main.run @@ fun env ->
  main (Eio.Buf_read.of_flow ~max_size:Int.max_int env#stdin) env#stdout
