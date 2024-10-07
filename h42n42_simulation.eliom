let%server service =
  Eliom_service.create ~path:(Eliom_service.Path ["simulation"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%client service = ~%service
let%shared effect () = H42n42_simulation_canvas.effect ()

let%shared page () () =
  Eliom_content.Html.F.(
    body
      [ div
          ~a:[a_class ["w-full flex justify-center bg-green-100"]]
          [H42n42_simulation_canvas.c ()] ])
