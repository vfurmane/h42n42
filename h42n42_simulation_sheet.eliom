let%shared c () =
  let creet_kinds =
    ["Healthy", "bg-purple-800", "Purple"; "Sick", "bg-rose-400", "Rose"]
  in
  Eliom_content.Html.F.(
    section
      ~a:[a_class ["px-6"; "py-4"; "min-w-64"; "bg-amber-50"]]
      [ article
          [ h2 ~a:[a_class ["mb-3"]] [txt "Caption"]
          ; ul
              (List.map
                 (fun (label, color_cn, color_name) ->
                    li
                      ~a:[a_class ["flex"; "[&:not(:last-child)]:mb-1"; "items-center"]]
                      [ div
                          ~a:
                            [ a_title (color_name ^ " color")
                            ; a_class
                                [ "inline-block"
                                ; "mr-1.5"
                                ; "size-4"
                                ; color_cn
                                ; "rounded" ] ]
                          []
                      ; txt (Format.sprintf "– %s creet" label) ])
                 creet_kinds) ] ])
