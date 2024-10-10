let%server service =
  Eliom_service.create ~path:(Eliom_service.Path ["simulation"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%client service = ~%service

let%shared page () () =
  let elt =
    Eliom_content.Html.F.(
      body
        [ main
            ~a:[a_class ["flex"; "justify-center"; "mt-20"; "w-full"]]
            [H42n42_simulation_frame.c ()] ])
  in
  elt
