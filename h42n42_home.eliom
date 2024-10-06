let%server service =
  Eliom_service.create ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%client service = ~%service

let%shared page () () =
  Lwt.return
    Eliom_content.Html.F.(
      html
        (head
           (title (txt "h42n42"))
           [ css_link
               ~uri:
                 (make_uri
                    ~service:(Eliom_service.static_dir ())
                    ["css"; "h42n42.css"])
               () ])
        (body
           (* TODO make this a component *)
           [H42n42_home_header.c ~cta_service:service ()]))
