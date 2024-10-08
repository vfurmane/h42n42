(* TODO move out *)
module type%client Creet = sig
  (* TODO doc all *)
  type t

  val rotation_prob : float
  val time_before_rotating : float
  val healthy : t

  (* TODO remove or change, too permissive *)
  val spawn : float -> float -> float -> float -> t
  val move : float -> float * float -> t -> t
  val get_x : t -> float
  val get_y : t -> float
  val get_radius : t -> float
  val rotate : timestamp:float -> float -> t -> t
  val can_rotate : timestamp:float -> t -> bool
end

module%client Creet : Creet = struct
  type t =
    { x : float
    ; y : float
    ; radius : float
    ; speed : float
    ; direction : float
    ; last_rotation_timestamp : float }

  let rotation_prob = 1. /. 25.
  let time_before_rotating = 1000. *. 2.

  let healthy =
    { x = 0.
    ; y = 0.
    ; radius = 35.
    ; speed = 15.
    ; direction = 1.7 *. Float.pi
    ; last_rotation_timestamp = 0. }

  let spawn x y speed direction =
    {x; y; radius = 35.; speed; direction; last_rotation_timestamp = 0.}

  let dir_to_coord a =
    let dx = cos a and dy = 0. -. sin a in
    dx, dy

  let move t (limit_x, limit_y) c =
    let dx, dy = dir_to_coord c.direction in
    let new_x = c.x +. (dx *. c.speed *. t)
    and new_y = c.y +. (dy *. c.speed *. t) in
    let new_dx =
      if (new_x -. c.radius <= 0.0 && dx < 0.0)
         || (new_x +. c.radius >= limit_x && dx > 0.0)
      then 0. -. dx
      else dx
    in
    let new_dy =
      if (new_y -. c.radius <= 0.0 && dy < 0.0)
         || (new_y +. c.radius >= limit_y && dy > 0.0)
      then 0. -. dy
      else dy
    in
    let new_d = Float.atan2 (0. -. new_dy) new_dx in
    {c with x = new_x; y = new_y; direction = new_d}

  let get_x c = c.x
  let get_y c = c.y
  let get_radius c = c.radius

  let rotate ~timestamp a c =
    {c with direction = c.direction +. a; last_rotation_timestamp = timestamp}
  (* TODO handle > pi || < -pi ---^ *)

  (* TODO perf bottleneck here? *)
  let can_rotate ~timestamp {last_rotation_timestamp} =
    last_rotation_timestamp +. time_before_rotating -. timestamp < 0.
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
  val get_limits : t -> int * int
  val get_flimits : t -> float * float
  val clear : t -> unit

  (* TODO named params *)
  val move_creet :
     timestamp:float
    -> float
    -> float * float
    -> Creet.t
    -> t
    -> Creet.t

  val draw_creet : t -> Creet.t -> unit
end

module%client SimulationCanvas : SimulationCanvas = struct
  open Js_of_ocaml

  type t =
    {ctx : Dom_html.canvasRenderingContext2D Js.t; ran_state : Random.State.t}

  let of_ctx ctx = {ctx; ran_state = Random.State.make_self_init ()}
  let get_width {ctx} = ctx##.canvas##.width
  let get_height {ctx} = ctx##.canvas##.height
  let get_fwidth {ctx} = float_of_int ctx##.canvas##.width
  let get_fheight {ctx} = float_of_int ctx##.canvas##.height
  let get_limits canvas = get_width canvas, get_height canvas

  let get_flimits canvas =
    float_of_int (get_width canvas), float_of_int (get_height canvas)

  let clear canvas =
    ignore
      canvas.ctx ## (clearRect 0. 0. (get_fwidth canvas) (get_fheight canvas))

  let move_creet ~timestamp t l creet {ran_state} =
    let is_rotating =
      if Creet.can_rotate ~timestamp creet
      then
        let rotation_prob = Random.State.float ran_state 1. in
        rotation_prob <= Creet.rotation_prob
      else false
    in
    let new_creet =
      if is_rotating
      then
        let random_direction =
          Random.State.float ran_state ((2. *. Float.pi) -. Float.pi)
        in
        Creet.rotate ~timestamp random_direction creet
      else creet
    in
    Creet.move t l new_creet

  let draw_creet {ctx} creet =
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
           let new_creet =
             SimulationCanvas.move_creet ~timestamp seconds_passed
               (SimulationCanvas.get_flimits sim_canvas)
               creet sim_canvas
           in
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
               [ Creet.spawn 0. 0. 115. (1.7 *. Float.pi)
               ; Creet.spawn 0. 100. 115. (1.94 *. Float.pi)
               ; Creet.spawn 0. 600. 115. (0.3 *. Float.pi)
               ; Creet.spawn 496. 578. 115. (0.9 *. Float.pi) ] } }
       (performance_now ()))

let%shared effect () = ignore [%client (init_client () : unit)]
let%shared c () = canvas_elt
