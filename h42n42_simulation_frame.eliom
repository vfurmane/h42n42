let%shared limit_x = 900.
let%shared limit_y = 675.
let%shared limits = limit_x, limit_y
let%shared river_limit_y = 28.
let%shared hospital_limit_y = 72.
let%shared base_creets_nbr = 3
let%shared sim_speed_rate = 1. /. 90.

let%client effect ~sim_speed ~(creets : Creet.M.t list) ~elt () =
  let open Js_of_ocaml in
  let game_over_elt = H42n42_simulation_frame_game_over.c () in
  let rec sim_loop ~sim ~last_update_timestamp () =
    let timestamp = (new%js Js.date_now)##getTime in
    let elapsed_time = (timestamp -. last_update_timestamp) /. 1000. in
    let sim =
      Simulation.M.random_spawn ~elt ~timestamp ~limits:~%limits
        ~hospital_limit_y:~%hospital_limit_y sim
      |> Simulation.M.update_speed ~elapsed_time ~speed_rate:sim_speed_rate
      |> Simulation.M.contaminate_creets |> Simulation.M.heal_creets
    in
    if Simulation.M.is_game_over sim
    then
      Lwt.return
        (Js_of_ocaml.Dom.appendChild
           (Eliom_content.Html.To_dom.of_element elt)
           (Eliom_content.Html.To_dom.of_element game_over_elt))
    else
      let%lwt _ = Js_of_ocaml_lwt.Lwt_js.sleep Defaults.refresh_rate in
      sim_loop ~sim ~last_update_timestamp:timestamp ()
  in
  ignore
    (Lwt.join
       [ sim_loop
           ~sim:
             (Simulation.M.start ~elt
                ~timestamp:(new%js Js.date_now)##getTime
                ~creets ~limits ~river_limit_y:~%river_limit_y ~hospital_limit_y
                ~speed:sim_speed ())
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
              [ "flex"
              ; "overflow-hidden"
              ; "relative"
              ; "flex-col"
              ; Format.sprintf "w-[%s]" (Utils.px_of_float limit_x)
              ; Format.sprintf "h-[%s]" (Utils.px_of_float limit_y)
              ; "select-none" ] ]
        [ div
            ~a:
              [ a_class ["bg-teal-800"]
              ; a_style
                  (Format.sprintf "height: %s"
                     (Utils.px_of_float river_limit_y)) ]
            []
        ; div ~a:[a_class ["grow"; "bg-green-300"]] []
        ; div
            ~a:
              [ a_class ["bg-slate-300"]
              ; a_style
                  (Format.sprintf "height: %s"
                     (Utils.px_of_float hospital_limit_y)) ]
            [] ])
  in
  let _ =
    [%client
      (effect ~sim_speed:~%sim_speed ~creets:~%creets ~elt:~%elt () : unit)]
  in
  elt
