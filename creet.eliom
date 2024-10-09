module type%shared M = sig
  type t

  val spawn : pos:float * float -> t
end

module%shared M = struct
  type t = {pos : float * float}

  let spawn pos = {pos}
end
