module type%client M = sig
  type t

  val start : speed:float ref -> creets:Creet.M.t list -> unit -> t

  val random_spawn :
     elt:Html_types.div Eliom_content.Html.F.elt
    -> timestamp:float
    -> limits:float * float
    -> t
    -> t
end

module%client M : M = struct
  type t =
    {speed : float ref; creets : Creet.M.t list; time_before_next_spawn : float}

  let start ~speed ~creets () = {speed; creets; time_before_next_spawn = 0.}

  let random_spawn ~elt ~timestamp ~limits sim =
    let is_spawning = timestamp > sim.time_before_next_spawn in
    if is_spawning
    then (
      let new_creet = Creet.M.ran_spawn ~sim_speed:sim.speed ~limits () in
      let seconds_before_new_spawn =
        Utils.random_float_in_range ~min:4.5 ~max:12.
      in
      Js_of_ocaml.Dom.appendChild
        (Eliom_content.Html.To_dom.of_element elt)
        (Eliom_content.Html.To_dom.of_element
           (H42n42_simulation_creet.c ~creet:new_creet ~limits ()));
      { sim with
        creets = new_creet :: sim.creets
      ; time_before_next_spawn =
          timestamp +. (seconds_before_new_spawn /. !(sim.speed) *. 1000.) })
    else sim
end
