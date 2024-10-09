let%server service =
  Eliom_service.create ~path:(Eliom_service.Path ["simulation"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%client service = ~%service

let%shared page () () =
  Eliom_content.Html.F.(
    body
      [ div
          ~a:
            [ a_class
                [ "overflow-hidden"
                ; "w-[900px]"
                ; "h-[675px]"
                ; "mt-20"
                ; "mx-auto"
                ; "bg-green-300" ] ]
          [] ])
