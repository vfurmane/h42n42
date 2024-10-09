module type%shared M = sig
  type t

  val spawn : pos:float * float -> t
  val get_pos : t -> float * float
  val get_radius : t -> float
  val move : elapsed_time:float -> t -> t
end

module%shared M = struct
  type t =
    {pos : float * float; radius : float; speed : float; direction : float}

  let spawn pos = {pos; radius = 24.; speed = 100.; direction = 0.}
  let get_pos {pos} = pos
  let get_radius {radius} = radius

  let move ~elapsed_time c =
    let x, y = get_pos c in
    {c with pos = x +. (c.speed *. elapsed_time), y +. (c.speed *. elapsed_time)}
end
