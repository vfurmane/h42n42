let%shared limit_x = 900.
let%shared limit_y = 675.
let%shared limits = limit_x, limit_y
let%shared base_creets_nbr = 5

let%client effect ~sim_speed ~creets ~elt () =
  let open Js_of_ocaml in
  let rec sim_loop ~sim ~last_update_timestamp () =
    let timestamp = (new%js Js.date_now)##getTime in
    let elapsed_time = (timestamp -. last_update_timestamp) /. 1000. in
    (* TODO remove *)
    ignore elapsed_time;
    let sim = Simulation.M.random_spawn ~elt ~timestamp ~limits:~%limits sim in
    let%lwt _ = Js_of_ocaml_lwt.Lwt_js.sleep Defaults.refresh_rate in
    sim_loop ~sim ~last_update_timestamp:timestamp ()
  in
  ignore
    (Lwt.join
       [ sim_loop
           ~sim:(Simulation.M.start ~creets ~speed:sim_speed ())
           ~last_update_timestamp:(new%js Js.date_now)##getTime
           () ])

let%shared c () =
  let sim_speed = ref 1. in
  let creets : 'a list =
    List.init base_creets_nbr (fun _ -> Creet.M.ran_spawn ~sim_speed ~limits ())
  in
  let elt =
    Eliom_content.Html.D.(
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
  let _ =
    [%client
      (effect ~sim_speed:~%sim_speed ~creets:~%creets ~elt:~%elt () : unit)]
  in
  elt
