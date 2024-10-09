let%shared tl_of_center (x, y) r = x -. r, y -. r

let%shared c ~creet () =
  let radius = Creet.M.get_radius creet in
  let x, y = tl_of_center (Creet.M.get_pos creet) radius in
  let px_of_float f = Format.sprintf "%dpx" (int_of_float f) in
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
