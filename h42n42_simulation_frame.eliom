let%shared limit_x = 900.
let%shared limit_y = 675.
let%shared limits = limit_x, limit_y
let%shared base_creets_nbr = 5

let%shared c () =
  let creets : 'a list =
    List.init base_creets_nbr (fun _ -> Creet.M.ran_spawn ~limits ())
  in
  let elt =
    Eliom_content.Html.F.(
      div
        ~a:
          [ a_class
              [ "overflow-hidden"
              ; "relative"
              ; Format.sprintf "w-[%s]" (Utils.px_of_float limit_x)
              ; Format.sprintf "h-[%s]" (Utils.px_of_float limit_y)
              ; "bg-green-300"
              ; "select-none" ] ]
        (List.map
           (fun creet -> H42n42_simulation_creet.c ~creet ~limits ())
           creets))
  in
  elt
