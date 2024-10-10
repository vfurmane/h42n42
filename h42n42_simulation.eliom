let%shared limit_x = 900.
let%shared limit_y = 675.
let%shared limits = limit_x, limit_y
let%shared base_creets_nbr = 5

let%server service =
  Eliom_service.create ~path:(Eliom_service.Path ["simulation"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%client service = ~%service

let%shared page () () =
  let creets : 'a list =
    List.init base_creets_nbr (fun _ -> Creet.M.ran_spawn ~limits ())
  in
  Eliom_content.Html.F.(
    body
      [ div
          ~a:
            [ a_class
                [ "overflow-hidden"
                ; "relative"
                ; Format.sprintf "w-[%s]" (Utils.px_of_float limit_x)
                ; Format.sprintf "h-[%s]" (Utils.px_of_float limit_y)
                ; "mt-20"
                ; "mx-auto"
                ; "bg-green-300"
                ; "select-none" ] ]
          (List.map
             (fun creet -> H42n42_simulation_creet.c ~creet ~limits ())
             creets) ])
