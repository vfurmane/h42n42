module type%client M = sig
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
    -> Creet.M.t
    -> t
    -> Creet.M.t

  val draw_creet : t -> Creet.M.t -> unit
end

module%client M : M = struct
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
      if Creet.M.can_rotate ~timestamp creet
      then
        let rotation_prob = Random.State.float ran_state 1. in
        rotation_prob <= Creet.M.rotation_prob
      else false
    in
    let new_creet =
      if is_rotating
      then
        let random_direction =
          Random.State.float ran_state ((2. *. Float.pi) -. Float.pi)
        in
        Creet.M.rotate ~timestamp random_direction creet
      else creet
    in
    Creet.M.move t l new_creet

  let draw_creet {ctx} creet =
    let x = Creet.M.get_x creet
    and y = Creet.M.get_y creet
    and r = Creet.M.get_radius creet in
    ctx##beginPath;
    ctx##.fillStyle := Js.string "#ff8080";
    ctx ## (arc x y r 0. (2. *. Float.pi) (Js.bool false));
    ctx##fill
end
