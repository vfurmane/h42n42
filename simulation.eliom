module type%client M = sig
  type t

  val start : unit -> t
end

module%client M : M = struct
  type t = {time_before_next_spawn : float}

  let start () = {time_before_next_spawn = 0.}
end
