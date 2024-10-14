let%client held_creet_class_name = "held-creet"

let%client effect ~creet:initial_creet ~limits ~hospital_limit_y ~elt () =
  let open Js_of_ocaml in
  let creet_elt = Eliom_content.Html.To_dom.of_element elt in
  let is_held = ref false in
  let parent_pos = ref (0., 0.) in
  let mouse_pos = ref (0, 0) in
  let rec creet_loop ~creet:creet_ref ~last_update_timestamp () =
    let timestamp = (new%js Js.date_now)##getTime in
    let elapsed_time = (timestamp -. last_update_timestamp) /. 1000. in
    let creet = !creet_ref in
    let radius = Creet.M.get_radius creet in
    let new_creet =
      if !is_held = true
      then
        let parent_x, parent_y = !parent_pos in
        let mouse_x, mouse_y = !mouse_pos in
        creet |> Creet.M.hold
        |> Creet.M.grow_berserk ~elapsed_time
        |> Creet.M.set_pos ~limits
             (float_of_int mouse_x -. parent_x, float_of_int mouse_y -. parent_y)
      else
        creet |> Creet.M.release
        |> Creet.M.grow_berserk ~elapsed_time
        |> Creet.M.move ~timestamp ~elapsed_time ~limits ~hospital_limit_y
    in
    let x, y =
      (Creet.M.get_pos new_creet |> Utils.tl_of_center)
        (radius +. Creet.M.grab_offset)
    in
    creet_elt##.style##.left := Js.string (Utils.px_of_float x);
    creet_elt##.style##.top := Js.string (Utils.px_of_float y);
    (let new_radius = Creet.M.get_radius new_creet in
     creet_elt##.style##.width
     := Js_of_ocaml.Js.string (Utils.px_of_float (new_radius *. 2.));
     creet_elt##.style##.height
     := Js_of_ocaml.Js.string (Utils.px_of_float (new_radius *. 2.)));
    Creet.M.update_color ~elt creet;
    creet_ref := new_creet;
    let%lwt _ = Js_of_ocaml_lwt.Lwt_js.sleep Defaults.refresh_rate in
    creet_loop ~creet:creet_ref ~last_update_timestamp:timestamp ()
  in
  ignore
    (Lwt.join
       [ creet_loop ~creet:initial_creet
           ~last_update_timestamp:(new%js Js.date_now)##getTime
           ()
       ; (let open Js_of_ocaml_lwt in
          Lwt_js_events.mousedowns creet_elt (fun ev _ ->
            let move_creet_to_mouse x y () =
              mouse_pos := x, y;
              ()
            in
            (parent_pos :=
               let parent_elt = creet_elt##.offsetParent in
               match Js.Opt.to_option parent_elt with
               | None -> 0., 0.
               | Some parent_elt ->
                   let bounding_rect = parent_elt##getBoundingClientRect in
                   bounding_rect##.left, bounding_rect##.top);
            let clientX = ev##.clientX and clientY = ev##.clientY in
            move_creet_to_mouse clientX clientY ();
            is_held := true;
            creet_elt##.classList##add (Js.string held_creet_class_name);
            Lwt.pick
              [ Lwt_js_events.mousemoves Dom_html.document (fun ev _ ->
                  let clientX = ev##.clientX and clientY = ev##.clientY in
                  move_creet_to_mouse clientX clientY ();
                  Lwt.return ())
              ; (let%lwt _ = Lwt_js_events.mouseup Dom_html.document in
                 is_held := false;
                 creet_elt##.classList##remove (Js.string held_creet_class_name);
                 Lwt.return ()) ])) ])

let%shared c ~creet:creet_ref ~(limits : float * float)
    ~(hospital_limit_y : float) ()
  =
  let creet = !creet_ref in
  let radius = Creet.M.get_radius creet in
  let x, y = Utils.tl_of_center (Creet.M.get_pos creet) radius in
  let elt =
    Eliom_content.Html.D.(
      div
        ~a:
          [ a_class ["absolute"; "rounded-full"; "cursor-grab"]
          ; a_style
              (Format.sprintf
                 "left: %s; top: %s; width: %s; height: %s; padding: %s"
                 (Utils.px_of_float x) (Utils.px_of_float y)
                 (Utils.px_of_float (radius *. 2.))
                 (Utils.px_of_float (radius *. 2.))
                 (Utils.px_of_float Creet.M.grab_offset)) ]
        [div ~a:[a_class ["size-full"; "rounded-full"]] []])
  in
  let _ =
    [%client
      (effect ~creet:~%creet_ref ~limits:~%limits
         ~hospital_limit_y:~%hospital_limit_y ~elt:~%elt ()
       : unit)]
  in
  elt
