let%shared size = 900.
let%shared aspect_ratio = 4. /. 3.
let%shared width = size
let%shared height = size /. aspect_ratio

type%shared rectangle = {x : float; y : float; speed : float}
type%shared draw_options_last_frame = {timestamp : float; rectangle : rectangle}
type%shared draw_options = {last_frame : draw_options_last_frame}

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

let%client performance_now () : float =
  let open Js_of_ocaml in
  let perf = Js.Unsafe.js_expr "performance" in
  Js.Unsafe.meth_call perf "now" [||]

let%client init_client () =
  let open Js_of_ocaml in
  let canvas = Eliom_content.Html.To_dom.of_canvas ~%canvas_elt in
  let ctx = canvas ## (getContext Dom_html._2d_) in
  let clear_frame () = ctx##clearRect 0. 0. width height in
  let rec draw_frame (options : draw_options) (timestamp : float) =
    let seconds_passed = (timestamp -. options.last_frame.timestamp) /. 1000. in
    let x =
      options.last_frame.rectangle.x
      +. (options.last_frame.rectangle.speed *. seconds_passed)
    and y =
      options.last_frame.rectangle.y
      +. (options.last_frame.rectangle.speed *. seconds_passed)
    in
    clear_frame ();
    ctx##.fillStyle := Js.string "#ff8080";
    ctx##fillRect x y 100. 100.;
    ignore
      (Dom_html.window##requestAnimationFrame
         (Js.wrap_callback
            (draw_frame
               { last_frame =
                   { timestamp
                   ; rectangle = {options.last_frame.rectangle with x; y} } })))
  in
  ignore
    (draw_frame
       {last_frame = {timestamp = 0.; rectangle = {x = 0.; y = 0.; speed = 10.}}}
       (performance_now ()))

let%shared effect () = ignore [%client (init_client () : unit)]
let%shared c () = canvas_elt
