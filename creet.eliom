module type%shared M = sig
  type t

  val spawn : pos:float * float -> t
  val get_pos : t -> float * float
  val get_radius : t -> float
  val move : elapsed_time:float -> limits:float * float -> t -> t
end

module%shared M = struct
  type t =
    {pos : float * float; radius : float; speed : float; direction : float}

  let rotation_prob = 1. /. 40.

  let spawn pos = {pos; radius = 24.; speed = 100.; direction = 0.}
  let get_pos {pos} = pos
  let get_radius {radius} = radius

  let dir_to_coord a =
    let dx = cos a and dy = 0. -. sin a in
    dx, dy

  let direction_with_randomness d =
    let is_rotating =
      let rotation_prob_random = Random.float 1. in
      rotation_prob_random <= rotation_prob
    in
    if is_rotating
    then
      let random_direction = Random.float ((2. *. Float.pi) -. Float.pi) in
      d +. random_direction
    else d

  let bump_on_limits ~limits:(limit_x, limit_y) (x, y) (dx, dy) r =
    let new_x, new_dx =
      let cond1 = x -. r <= 0. && dx < 0. in
      if cond1 || (x +. r >= limit_x && dx > 0.)
      then
        let new_x = if cond1 then r else limit_x -. r in
        new_x, 0. -. dx
      else x, dx
    in
    let new_y, new_dy =
      let cond1 = y -. r <= 0. && dy < 0. in
      if cond1 || (y +. r >= limit_y && dy > 0.)
      then
        let new_y = if cond1 then r else limit_y -. r in
        new_y, 0. -. dy
      else y, dy
    in
    new_x, new_y, new_dx, new_dy

  let move ~elapsed_time ~limits:(limit_x, limit_y) c =
    let new_direction = direction_with_randomness c.direction in
    let x, y = get_pos c in
    let dx, dy = dir_to_coord new_direction in
    let new_x = x +. (dx *. c.speed *. elapsed_time) in
    let new_y = y +. (dy *. c.speed *. elapsed_time) in
    let new_x, new_y, new_dx, new_dy =
      bump_on_limits ~limits:(limit_x, limit_y) (new_x, new_y) (dx, dy)
        (get_radius c)
    in
    let new_direction = Float.atan2 (0. -. new_dy) new_dx in
    {c with pos = new_x, new_y; direction = new_direction}
end
