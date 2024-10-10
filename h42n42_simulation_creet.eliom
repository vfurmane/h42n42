let%client refresh_rate = 1. /. 60.
let%client held_creet_class_name = "held-creet"

let%client effect ~creet:initial_creet ~limits ~elt () =
  let open Js_of_ocaml in
  let creet_elt = Eliom_content.Html.To_dom.of_element elt in
  let is_held = ref false in
  let rec creet_loop ~creet ~last_update_timestamp () =
    let timestamp = (new%js Js.date_now)##getTime in
    let elapsed_time = (timestamp -. last_update_timestamp) /. 1000. in
    let radius = Creet.M.get_radius creet in
    let new_creet =
      if !is_held = true
      then creet
      else
        let new_creet = Creet.M.move ~timestamp ~elapsed_time ~limits creet in
        let x, y = (Creet.M.get_pos new_creet |> Utils.tl_of_center) radius in
        creet_elt##.style##.left := Js.string (Utils.px_of_float x);
        creet_elt##.style##.top := Js.string (Utils.px_of_float y);
        new_creet
    in
    let%lwt _ = Js_of_ocaml_lwt.Lwt_js.sleep refresh_rate in
    creet_loop ~creet:new_creet ~last_update_timestamp:timestamp ()
  in
  ignore
    (Lwt.join
       [ creet_loop ~creet:initial_creet
           ~last_update_timestamp:(new%js Js.date_now)##getTime
           ()
       ; (let open Js_of_ocaml_lwt in
          Lwt_js_events.mousedowns creet_elt (fun ev _ ->
            is_held := true;
            creet_elt##.classList##add (Js.string held_creet_class_name);
            Firebug.console##log
              (Format.sprintf "x: %d; y: %d" ev##.clientX ev##.clientY);
            Lwt.pick
              [ (let%lwt _ = Lwt_js_events.mouseup Dom_html.document in
                 is_held := false;
                 creet_elt##.classList##remove (Js.string held_creet_class_name);
                 Lwt.return ()) ])) ])

let%shared c ~creet ~(limits : float * float) () =
  let radius = Creet.M.get_radius creet in
  let x, y = Utils.tl_of_center (Creet.M.get_pos creet) radius in
  let elt =
    Eliom_content.Html.D.(
      div
        ~a:
          [ a_class ["absolute"; "bg-purple-800"; "rounded-full"]
          ; a_style
              (Format.sprintf "left: %s; top: %s; width: %s; height: %s"
                 (Utils.px_of_float x) (Utils.px_of_float y)
                 (Utils.px_of_float (radius *. 2.))
                 (Utils.px_of_float (radius *. 2.))) ]
        [])
  in
  let _ =
    [%client (effect ~creet:~%creet ~limits:~%limits ~elt:~%elt () : unit)]
  in
  elt
