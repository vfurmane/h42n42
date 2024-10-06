let%server service =
  Eliom_service.create ~path:(Eliom_service.Path ["simulation"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%client service = ~%service
let%shared page () () = Eliom_content.Html.F.(body [h1 [txt "Hello, World!"]])
