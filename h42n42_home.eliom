let%server service =
  Eliom_service.create ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%client service = ~%service

let%shared page () () =
  Eliom_content.Html.F.body [H42n42_home_header.c ~cta_service:service ()]
