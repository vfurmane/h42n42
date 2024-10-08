module type%client M = sig
  (* TODO doc all *)
  type t

  val rotation_prob : float
  val time_before_rotating : float
  val healthy : t

  (* TODO use named parameters *)
  (* TODO remove or change, too permissive *)
  val spawn : float -> float -> float -> float -> t
  val move : float -> float * float -> t -> t
  val get_x : t -> float
  val get_y : t -> float
  val get_radius : t -> float
  val rotate : timestamp:float -> float -> t -> t
  val can_rotate : timestamp:float -> t -> bool
end

module%client M : M = struct
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
