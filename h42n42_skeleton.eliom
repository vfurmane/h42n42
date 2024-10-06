let%shared c ~title ~page () () =
  Lwt.return
    (Eliom_tools.D.html ~title ~css:[["css"; "h42n42.css"]] (page () ()))
