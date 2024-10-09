let%shared c ~creet () =
  let x, y = Creet.M.get_pos creet in
  let px_of_float f = Format.sprintf "%dpx" (int_of_float f) in
  Eliom_content.Html.D.(
    div
      ~a:
        [ a_class ["absolute"; "size-12"; "bg-purple-800"; "rounded-full"]
        ; a_style
            (Format.sprintf "left: %s; top: %s" (px_of_float x) (px_of_float y))
        ]
      [])
