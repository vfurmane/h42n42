module type%shared M = sig
  type t

  val spawn : pos:float * float -> t
  val get_pos : t -> float * float
  val get_radius : t -> float
end

module%shared M = struct
  type t =
    {pos : float * float; radius : float; speed : float; direction : float}

  let spawn pos = {pos; radius = 24.; speed = 100.; direction = 0.}
  let get_pos {pos} = pos
  let get_radius {radius} = radius
end
