let%shared size = 900.
let%shared aspect_ratio = 4. /. 3.
let%shared width = size
let%shared height = size /. aspect_ratio

type%client draw_options_last_frame = {timestamp : float}
type%client draw_options = {last_frame : draw_options_last_frame}

let%shared canvas_elt =
  Eliom_content.Html.D.(
    canvas
      ~a:
        [ a_width (int_of_float width)
        ; a_height (int_of_float height)
        ; a_class ["bg-white"] ]
      [ h2 [txt "The simulation"]
      ; p
          [ txt
              "The creets are simulated here, unfortunately, your browser does not seem to support HTML5 canvas."
          ] ])

let%client init_client () =
  let open Js_of_ocaml in
  let canvas = Eliom_content.Html.To_dom.of_canvas ~%canvas_elt in
  let ctx = canvas##getContext Dom_html._2d_ in
  let sim_canvas = Simulation_canvas.M.of_ctx ctx in
  let creets =
    ref
      [ Creet.M.spawn 0. 0. 115. (1.7 *. Float.pi)
      ; Creet.M.spawn 0. 100. 115. (1.94 *. Float.pi)
      ; Creet.M.spawn 0. 600. 115. (0.3 *. Float.pi)
      ; Creet.M.spawn 496. 578. 115. (0.9 *. Float.pi) ]
  in
  let rec draw_frame (options : draw_options) (timestamp : float) =
    let seconds_passed = (timestamp -. options.last_frame.timestamp) /. 1000. in
    let _ = Simulation_canvas.M.clear sim_canvas in
    let new_creets =
      List.map
        (fun creet ->
           let new_creet =
             Simulation_canvas.M.move_creet ~timestamp seconds_passed
               (Simulation_canvas.M.get_flimits sim_canvas)
               creet sim_canvas
           in
           Simulation_canvas.M.draw_creet sim_canvas new_creet;
           new_creet)
        !creets
    in
    creets := new_creets;
    ignore
      (Dom_html.window##requestAnimationFrame
         (Js.wrap_callback (draw_frame {last_frame = {timestamp}})))
  in
  (* TODO use request animation and get rid of performance_now *)
  ignore
    (draw_frame {last_frame = {timestamp = 0.}} (Unsafe_js.performance_now ()));
  Lwt.async (fun () ->
    let open Js_of_ocaml_lwt in
    Lwt_js_events.mousedowns canvas (fun ev _ ->
      Lwt.return
        (Firebug.console##log
           (Format.sprintf "x: %d; y: %d" ev##.clientX ev##.clientY))
      (* let%lwt () = line ev in
      Lwt.pick
        [ Lwt_js_events.mousemoves Dom_html.document (fun x _ -> line x)
        ; (let%lwt ev = Lwt_js_events.mouseup Dom_html.document in
           line ev) ] *)))

let%shared effect () = ignore [%client (init_client () : unit)]
let%shared c () = canvas_elt
