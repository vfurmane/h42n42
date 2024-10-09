module type%shared M = sig
  type t

  val spawn : pos:float * float -> t
  val get_pos : t -> float * float
end

module%shared M = struct
  type t = {pos : float * float}

  let spawn pos = {pos}
  let get_pos {pos} = pos
end
