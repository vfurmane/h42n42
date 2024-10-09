let%client refresh_rate = 1. /. 60.
let%shared tl_of_center (x, y) r = x -. r, y -. r
let%shared px_of_float f = Format.sprintf "%dpx" (int_of_float f)

let%client effect ~creet:initial_creet ~elt () =
  let open Js_of_ocaml in
  let creet_elt = Eliom_content.Html.To_dom.of_element elt in
  let rec creet_loop ~creet ~last_update_timestamp () =
    let timestamp = (new%js Js.date_now)##getTime in
    let elapsed_time = (timestamp -. last_update_timestamp) /. 1000. in
    let radius = Creet.M.get_radius creet in
    let new_creet = Creet.M.move ~elapsed_time creet in
    let x, y = (Creet.M.get_pos new_creet |> tl_of_center) radius in
    creet_elt##.style##.left := Js.string (px_of_float x);
    creet_elt##.style##.top := Js.string (px_of_float y);
    let%lwt _ = Js_of_ocaml_lwt.Lwt_js.sleep refresh_rate in
    creet_loop ~creet:new_creet ~last_update_timestamp:timestamp ()
  in
  Lwt.async
    (creet_loop ~creet:initial_creet
       ~last_update_timestamp:(new%js Js.date_now)##getTime)

let%shared c ~creet () =
  let radius = Creet.M.get_radius creet in
  let x, y = tl_of_center (Creet.M.get_pos creet) radius in
  let elt =
    Eliom_content.Html.D.(
      div
        ~a:
          [ a_class ["absolute"; "bg-purple-800"; "rounded-full"]
          ; a_style
              (Format.sprintf "left: %s; top: %s; width: %s; height: %s"
                 (px_of_float x) (px_of_float y)
                 (px_of_float (radius *. 2.))
                 (px_of_float (radius *. 2.))) ]
        [])
  in
  let _ = [%client (effect ~creet:~%creet ~elt:~%elt () : unit)] in
  elt
