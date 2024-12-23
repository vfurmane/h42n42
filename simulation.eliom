module type%client M = sig
  type t

  val start :
     elt:Html_types.div Eliom_content.Html.F.elt
    -> timestamp:float
    -> speed:float ref
    -> limits:float * float
    -> river_limit_y:float
    -> hospital_limit_y:float
    -> creets:Creet.M.t list ref
    -> unit
    -> t

  val random_spawn :
     elt:Html_types.div Eliom_content.Html.F.elt
    -> timestamp:float
    -> creets:Creet.M.t list ref
    -> limits:float * float
    -> hospital_limit_y:float
    -> t
    -> t

  val update_speed : elapsed_time:float -> speed_rate:float -> t -> t
  val contaminate_creets : t -> t
  val heal_creets : t -> t
  val is_game_over : t -> bool
  val get_creets : t -> Creet.M.t list
end

module%client M : M = struct
  type t =
    { speed : float ref
    ; limits : float * float
    ; river_limit_y : float
    ; hospital_limit_y : float
    ; creets :
        (Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t * Creet.M.t ref) list
    ; time_before_next_spawn : float }

  let start ~elt ~timestamp ~speed ~limits ~river_limit_y ~hospital_limit_y
      ~creets ()
    =
    let seconds_before_new_spawn =
      Utils.random_float_in_range ~min:7.5 ~max:16.
    in
    let creets =
      List.map
        (fun creet ->
           let new_creet_ref = ref creet in
           let new_creet_elt =
             Eliom_content.Html.To_dom.of_element
               (H42n42_simulation_creet.c ~creets ~creet:new_creet_ref ~limits
                  ~hospital_limit_y ())
           in
           Js_of_ocaml.Dom.appendChild
             (Eliom_content.Html.To_dom.of_element elt)
             new_creet_elt;
           new_creet_elt, new_creet_ref)
        !creets
    in
    { speed
    ; limits
    ; river_limit_y
    ; hospital_limit_y
    ; creets
    ; time_before_next_spawn = timestamp +. (seconds_before_new_spawn *. 1000.)
    }

  let random_spawn ~elt ~timestamp ~creets ~limits ~hospital_limit_y sim =
    let is_spawning = timestamp > sim.time_before_next_spawn in
    if is_spawning
    then (
      let new_creet = Creet.M.ran_spawn ~sim_speed:sim.speed ~limits () in
      let new_creet_ref = ref new_creet in
      let seconds_before_new_spawn =
        Utils.random_float_in_range ~min:7.5 ~max:16.
      in
      let new_creet_elt =
        Eliom_content.Html.To_dom.of_element
          (H42n42_simulation_creet.c ~creets ~creet:new_creet_ref ~limits
             ~hospital_limit_y ())
      in
      Js_of_ocaml.Dom.appendChild
        (Eliom_content.Html.To_dom.of_element elt)
        new_creet_elt;
      { sim with
        creets = (new_creet_elt, new_creet_ref) :: sim.creets
      ; time_before_next_spawn =
          timestamp +. (seconds_before_new_spawn /. !(sim.speed) *. 1000.) })
    else sim

  let update_speed ~elapsed_time ~speed_rate sim =
    sim.speed := !(sim.speed) +. (elapsed_time *. speed_rate);
    sim

  let contaminate_creets sim =
    let creets = sim.creets in
    let raw_creets = List.map (fun (_, creet) -> !creet) creets in
    let new_creets =
      List.map
        (fun (creet_elt, creet_ref) ->
           let creet = !creet_ref in
           let new_creet =
             Creet.M.contaminate_by_river_touch ~river_limit_y:sim.river_limit_y
               creet
             |> Creet.M.contaminate_by_sick_touch raw_creets
           in
           creet_ref := new_creet;
           creet_elt, creet_ref)
        creets
    in
    {sim with creets = new_creets}

  let heal_creets sim =
    let creets = sim.creets in
    let new_creets =
      List.map
        (fun (creet_elt, creet_ref) ->
           let creet = !creet_ref in
           let new_creet =
             Creet.M.heal_by_hospital_touch ~limit_y:(snd sim.limits)
               ~hospital_limit_y:sim.hospital_limit_y creet
           in
           creet_ref := new_creet;
           creet_elt, creet_ref)
        creets
    in
    {sim with creets = new_creets}

  let is_game_over {creets} =
    List.exists
      (fun (_, creet_ref) ->
         let creet = !creet_ref in
         Creet.M.is_healthy creet)
      creets
    = false

  let get_creets {creets} = List.map (fun (_, creet_ref) -> !creet_ref) creets
end
