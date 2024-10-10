module type%client M = sig
  type t

  val start : creets:Creet.M.t list -> unit -> t
end

module%client M : M = struct
  type t = {creets : Creet.M.t list; time_before_next_spawn : float}

  let start ~creets () = {creets; time_before_next_spawn = 0.}
end
