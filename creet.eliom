module type%shared M = sig
  type t

  val grab_offset : float
  val spawn : sim_speed:float -> pos:float * float -> t
  val ran_spawn : sim_speed:float -> limits:float * float -> unit -> t
  val get_pos : t -> float * float
  val get_radius : t -> float

  val move :
     timestamp:float
    -> elapsed_time:float
    -> t list ref
    -> limits:float * float
    -> t
    -> t

  val set_pos : float * float -> t -> t
  val contaminate_by_river_touch : river_limit_y:float -> t -> t
  val update_color : elt:Html_types.div Eliom_content.Html.F.elt -> t -> t
  val heal_by_hospital_touch : hospital_limit_y:float -> t -> t
  val is_healthy : t -> t
  val hold : t -> t
  val release : t -> t
  val grow_berserk : t -> t
end

module%shared M = struct
  let healthy_creet_class_name = "healthy-creet"
  let sick_creet_class_name = "sick-creet"
  let berserk_creet_class_name = "berserk-creet"
  let mean_creet_class_name = "mean-creet"

  type sick_kind = Normal | Berserk | Mean
  type kind = Healthy | Sick of sick_kind

  type t =
    { sim_speed : float ref
    ; kind : kind
    ; pos : float * float
    ; radius : float
    ; speed : float
    ; direction : float
    ; is_held : bool
    ; time_before_next_rotation : float }

  let grab_offset = 20.
  let healthy_radius = 24.
  let healthy_speed = 100.
  let sick_speed = healthy_speed *. (1. -. 0.15)
  let sick_radius = healthy_radius
  let berserk_radius = healthy_radius *. 4.
  let berserk_growing_rate = 2.
  let mean_radius = healthy_radius *. (1. -. 0.15)
  let rotation_prob = 1. /. 40.
  let contaminate_prob = 2. /. 100.
  let become_berserk_prob = 10. /. 100.
  let become_mean_prob = 10. /. 100.

  let ran_spawn ~sim_speed ~limits:(limit_x, limit_y) () =
    let radius = healthy_radius in
    let pos_limit_x = limit_x -. radius and pos_limit_y = limit_y -. radius in
    let pos =
      ( Utils.random_float_in_range ~min:radius ~max:pos_limit_x
      , Utils.random_float_in_range ~min:radius ~max:pos_limit_y )
    in
    let speed = healthy_speed in
    let direction = Random.float (2. *. Float.pi) in
    let time_before_next_rotation = 0. in
    { sim_speed
    ; kind = Healthy
    ; pos
    ; radius
    ; speed
    ; direction
    ; is_held = false
    ; time_before_next_rotation }

  let spawn ~sim_speed pos =
    { sim_speed
    ; kind = Healthy
    ; pos
    ; radius = healthy_radius
    ; speed = healthy_speed
    ; direction = 1.63577 *. Float.pi
    ; is_held = false
    ; time_before_next_rotation = 0. }

  let get_pos {pos} = pos
  let get_radius {radius} = radius

  let rotate ~timestamp d c =
    let seconds_before_next_rotation =
      Utils.random_float_in_range ~min:1.5 ~max:3.5
    in
    { c with
      direction = c.direction +. d
    ; time_before_next_rotation =
        timestamp +. (seconds_before_next_rotation *. 1000.) }

  let dir_to_coord a =
    let dx = cos a and dy = 0. -. sin a in
    dx, dy

  let random_rotation ~timestamp c =
    let is_rotating =
      if timestamp < c.time_before_next_rotation
      then false
      else
        let rotation_prob_random = Random.float 1. in
        rotation_prob_random <= rotation_prob
    in
    if is_rotating
    then
      let random_direction = Random.float ((2. *. Float.pi) -. Float.pi) in
      rotate ~timestamp random_direction c
    else c

  let bump_on_limits ~limits:(limit_x, limit_y) ~hospital_limit_y (x, y)
      (dx, dy) r
    =
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
      if cond1 || (y +. r >= limit_y -. hospital_limit_y && dy > 0.)
      then
        let new_y =
          if cond1 then r else if y +. r < limit_y then y else limit_y -. r
        in
        new_y, 0. -. dy
      else y, dy
    in
    new_x, new_y, new_dx, new_dy

  let normalize_direction (dx, dy) =
    let magnitude = sqrt ((dx *. dx) +. (dy *. dy)) in
    if magnitude = 0.0
    then 0.0, 0.0 (* Handle zero-length vector *)
    else dx /. magnitude, dy /. magnitude

  let natural_move ~timestamp ~elapsed_time ~limits:(limit_x, limit_y)
      ~hospital_limit_y c
    =
    let c = random_rotation ~timestamp c in
    let x, y = get_pos c in
    let dx, dy = dir_to_coord c.direction in
    let new_x = x +. (dx *. c.speed *. !(c.sim_speed) *. elapsed_time) in
    let new_y = y +. (dy *. c.speed *. !(c.sim_speed) *. elapsed_time) in
    let new_x, new_y, new_dx, new_dy =
      bump_on_limits ~limits:(limit_x, limit_y) ~hospital_limit_y (new_x, new_y)
        (dx, dy) (get_radius c)
    in
    let new_direction = Float.atan2 (0. -. new_dy) new_dx in
    {c with pos = new_x, new_y; direction = new_direction}

  let move ~timestamp ~elapsed_time ~creets ~limits:(limit_x, limit_y)
      ~hospital_limit_y c
    =
    if c.kind = Sick Mean
    then
      let nearest_creet =
        List.fold_left
          (fun nearest_creet creet ->
             if creet = c
             then nearest_creet
             else
               match creet.kind with
               | Healthy ->
                   if let _, y = get_pos creet in
                      y < limit_y -. hospital_limit_y
                   then Some creet
                   else nearest_creet
               | Sick _ -> nearest_creet)
          None !creets
      in
      match nearest_creet with
      | None ->
          natural_move ~timestamp ~elapsed_time ~limits:(limit_x, limit_y)
            ~hospital_limit_y c
      | Some nearest_creet ->
          let x, y = get_pos c in
          let target_x, target_y = get_pos nearest_creet in
          let new_dx, new_dy = target_x -. x, target_y -. y in
          let new_dx, new_dy = normalize_direction (new_dx, new_dy) in
          let new_direction = Float.atan2 (0. -. new_dy) new_dx in
          let new_dx, new_dy = dir_to_coord new_direction in
          let new_x =
            x +. (new_dx *. c.speed *. !(c.sim_speed) *. elapsed_time)
          in
          let new_y =
            y +. (new_dy *. c.speed *. !(c.sim_speed) *. elapsed_time)
          in
          let new_y =
            if new_y +. c.radius >= limit_y -. hospital_limit_y
            then limit_y -. hospital_limit_y -. c.radius
            else new_y
          in
          {c with pos = new_x, new_y; direction = new_direction}
    else
      natural_move ~timestamp ~elapsed_time ~limits:(limit_x, limit_y)
        ~hospital_limit_y c

  let set_pos ~limits:(limit_x, limit_y) (x, y) c =
    let r = c.radius in
    let new_x =
      let cond1 = x -. r <= 0. in
      if cond1 || x +. r >= limit_x
      then if cond1 then r else limit_x -. r
      else x
    in
    let new_y =
      let cond1 = y -. r <= 0. in
      if cond1 || y +. r >= limit_y
      then if cond1 then r else limit_y -. r
      else y
    in
    {c with pos = new_x, new_y}

  let rand_kind () =
    let kind_prob_random = Random.float 1. in
    if kind_prob_random <= become_berserk_prob
    then Berserk
    else if kind_prob_random <= become_berserk_prob +. become_mean_prob
    then Mean
    else Normal

  let contaminate kind c =
    let radius = if kind = Mean then mean_radius else sick_radius in
    {c with kind = Sick kind; radius}

  let contaminate_by_river_touch ~river_limit_y c =
    let is_contaminated =
      c.kind = Healthy && c.is_held = false
      &&
      let _, y = c.pos and radius = c.radius in
      y -. radius <= river_limit_y
    in
    if is_contaminated
    then
      let kind = rand_kind () in
      contaminate kind c
    else c

  let are_creets_touching c1 c2 =
    let are_creets_touching_axis a1 a2 r1 r2 =
      (a1 -. r1 < a2 +. r2 && a1 -. r1 > a2 -. r2)
      || (a1 +. r1 > a2 -. r2 && a1 +. r1 < a2 +. r2)
    in
    let x1, y1 = get_pos c1
    and x2, y2 = get_pos c2
    and r1 = c1.radius
    and r2 = c2.radius in
    are_creets_touching_axis x1 x2 r1 r2 && are_creets_touching_axis y1 y2 r1 r2

  let contaminate_by_sick_touch creets c =
    let is_contaminated =
      c.kind = Healthy && c.is_held = false && c.is_held = false
      && List.exists
           (fun creet ->
              match creet.kind with
              | Healthy -> false
              | Sick _ ->
                  are_creets_touching creet c
                  &&
                  let contaminate_prob_random = Random.float 1. in
                  contaminate_prob_random <= contaminate_prob)
           creets
    in
    if is_contaminated
    then
      let kind = rand_kind () in
      contaminate kind c
    else c

  let match_class_name c =
    match c.kind with
    | Healthy -> healthy_creet_class_name
    | Sick sk -> (
      match sk with
      | Normal -> sick_creet_class_name
      | Berserk -> berserk_creet_class_name
      | Mean -> mean_creet_class_name)

  let update_color ~(elt : Html_types.div Eliom_content.Html.F.elt) c =
    ignore elt;
    ignore
      [%client
        (let creet_elt = Eliom_content.Html.To_dom.of_element ~%elt in
         creet_elt##.classList##remove
           (Js_of_ocaml.Js.string ~%healthy_creet_class_name);
         creet_elt##.classList##remove
           (Js_of_ocaml.Js.string ~%sick_creet_class_name);
         creet_elt##.classList##remove
           (Js_of_ocaml.Js.string ~%berserk_creet_class_name);
         creet_elt##.classList##remove
           (Js_of_ocaml.Js.string ~%mean_creet_class_name);
         creet_elt##.classList##add
           (Js_of_ocaml.Js.string ~%(match_class_name c))
         : unit)]

  let heal_by_hospital_touch ~limit_y ~hospital_limit_y c =
    let is_healed =
      let _, y = c.pos in
      if c.radius <= hospital_limit_y
      then y >= limit_y -. hospital_limit_y
      else y +. c.radius >= limit_y
    in
    if is_healed
    then {c with kind = Healthy; radius = healthy_radius; speed = healthy_speed}
    else c

  let is_healthy c = c.kind = Healthy
  let hold c = {c with is_held = true}
  let release c = {c with is_held = false}

  let grow_berserk ~elapsed_time c =
    if c.kind = Sick Berserk && c.radius < berserk_radius
    then
      { c with
        radius =
          c.radius +. (berserk_growing_rate *. !(c.sim_speed) *. elapsed_time)
      }
    else c
end
