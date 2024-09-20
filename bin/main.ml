let myservice =
  Eliom_service.create
    ~path:(Eliom_service.Path [ "foo" ])
    ~meth:(Eliom_service.Get Eliom_parameter.(string "myparam" ** int "i"))
    ()
;;

let () =
  Eliom_registration.Html.register ~service:myservice (fun (myparam, _i) () ->
    Lwt.return
      Eliom_content.Html.F.(html (head (title (txt "")) []) (body [ h1 [ txt myparam ] ])))
;;

let () =
  Ocsigen_server.start
    ~command_pipe:"local/var/run/mysite-cmd"
    ~logdir:"local/var/log/mysite"
    ~datadir:"local/var/data/mysite"
    ~default_charset:(Some "utf-8")
    [ Ocsigen_server.host [ Staticmod.run ~dir:"local/var/www/mysite" (); Eliom.run () ] ]
;;
