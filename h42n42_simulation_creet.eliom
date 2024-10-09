let%shared c ~creet () =
  let x, y = Creet.M.get_pos creet in
  Eliom_content.Html.D.(p [txt (Format.sprintf "x=%f; y=%f" x y)])
