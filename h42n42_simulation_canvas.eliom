(* TODO move out *)
module type%client Creet = sig
  (* TODO doc all *)
  type t

  val healthy : t

  (* TODO remove or change, too permissive *)
  val spawn : float -> float -> float -> float -> t
  val move : float -> t -> t
  val get_x : t -> float
  val get_y : t -> float
  val get_radius : t -> float
end

module%client Creet : Creet = struct
  type t =
    {x : float; y : float; radius : float; speed : float; direction : float}

  let healthy =
    {x = 0.; y = 0.; radius = 50.; speed = 15.; direction = 1.7 *. Float.pi}

  let spawn x y speed direction = {x; y; radius = 50.; speed; direction}

  let dir_to_coord a =
    let dx = cos a and dy = 0. -. sin a in
    dx, dy

  (* TODO canvas width/height *)
  let move t c =
    let dx, dy = dir_to_coord c.direction in
    let new_x = c.x +. (dx *. c.speed *. t)
    and new_y = c.y +. (dy *. c.speed *. t) in
    let new_dx =
      if (new_x -. c.radius <= 0.0 && dx < 0.0)
         || (new_x +. c.radius >= 900. && dx > 0.0)
      then 0. -. dx
      else dx
    in
    let new_dy =
      if (new_y -. c.radius <= 0.0 && dy < 0.0)
         || (new_y +. c.radius >= 675. && dy > 0.0)
      then 0. -. dy
      else dy
    in
    let new_d = Float.atan2 (0. -. new_dy) new_dx in
    {c with x = new_x; y = new_y; direction = new_d}

  let get_x c = c.x
  let get_y c = c.y
  let get_radius c = c.radius
end

module type%client SimulationCanvas = sig
  (* TODO doc all *)
  type t

  val of_ctx :
     Js_of_ocaml.Dom_html.canvasRenderingContext2D Js_of_ocaml.Js.t
    -> t

  val get_width : t -> int
  val get_height : t -> int
  val get_fwidth : t -> float
  val get_fheight : t -> float
  val clear : t -> unit
  val draw_creet : t -> Creet.t -> unit
end

module%client SimulationCanvas : SimulationCanvas = struct
  open Js_of_ocaml

  type t = Dom_html.canvasRenderingContext2D Js.t

  let of_ctx ctx = ctx
  let get_width ctx = ctx##.canvas##.width
  let get_height ctx = ctx##.canvas##.height
  let get_fwidth ctx = float_of_int ctx##.canvas##.width
  let get_fheight ctx = float_of_int ctx##.canvas##.height

  let clear ctx =
    ignore ctx ## (clearRect 0. 0. (get_fwidth ctx) (get_fheight ctx))

  let draw_creet ctx creet =
    let x = Creet.get_x creet
    and y = Creet.get_y creet
    and r = Creet.get_radius creet in
    ctx##beginPath;
    ctx##.fillStyle := Js.string "#ff8080";
    ctx ## (arc x y r 0. (2. *. Float.pi) (Js.bool false));
    ctx##fill
end

let%shared size = 900.
let%shared aspect_ratio = 4. /. 3.
let%shared width = size
let%shared height = size /. aspect_ratio

type%client draw_options_last_frame = {timestamp : float; creets : Creet.t list}
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

let%client performance_now () : float =
  let open Js_of_ocaml in
  let perf = Js.Unsafe.js_expr "performance" in
  Js.Unsafe.meth_call perf "now" [||]

let%client init_client () =
  let open Js_of_ocaml in
  let canvas = Eliom_content.Html.To_dom.of_canvas ~%canvas_elt in
  let ctx = canvas##getContext Dom_html._2d_ in
  let sim_canvas = SimulationCanvas.of_ctx ctx in
  let rec draw_frame (options : draw_options) (timestamp : float) =
    let seconds_passed = (timestamp -. options.last_frame.timestamp) /. 1000. in
    let _ = SimulationCanvas.clear sim_canvas in
    let new_creets =
      List.map
        (fun creet ->
           let new_creet = Creet.move seconds_passed creet in
           SimulationCanvas.draw_creet sim_canvas new_creet;
           new_creet)
        options.last_frame.creets
    in
    ignore
      (Dom_html.window##requestAnimationFrame
         (Js.wrap_callback
            (draw_frame {last_frame = {timestamp; creets = new_creets}})))
  in
  ignore
    (draw_frame
       { last_frame =
           { timestamp = 0.
           ; creets =
               [ Creet.spawn 0. 0. 400. (1.7 *. Float.pi)
               ; Creet.spawn 0. 100. 400. (1.94 *. Float.pi)
               ; Creet.spawn 0. 600. 400. (0.3 *. Float.pi)
               ; Creet.spawn 496. 578. 400. (0.9 *. Float.pi) ] } }
       (performance_now ()))

let%shared effect () = ignore [%client (init_client () : unit)]
let%shared c () = canvas_elt
