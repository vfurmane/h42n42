let%shared c ~title ~page ?effect () () =
  let _ = match effect with None -> () | Some fn -> fn () in
  Lwt.return
    (Eliom_tools.D.html ~title ~css:[["css"; "h42n42.css"]] (page () ()))
