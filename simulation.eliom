module type%client M = sig
  type t

  val start : creets:Creet.M.t list -> unit -> t

  val random_spawn :
     elt:Html_types.div Eliom_content.Html.F.elt
    -> limits:float * float
    -> t
    -> t
end

module%client M : M = struct
  type t = {creets : Creet.M.t list; time_before_next_spawn : float}

  let start ~creets () = {creets; time_before_next_spawn = 0.}

  let random_spawn ~elt ~limits sim =
    let new_creet = Creet.M.ran_spawn ~limits () in
    Js_of_ocaml.Dom.appendChild
      (Eliom_content.Html.To_dom.of_element elt)
      (Eliom_content.Html.To_dom.of_element
         (H42n42_simulation_creet.c ~creet:new_creet ~limits ()));
    {sim with creets = new_creet :: sim.creets}
end
