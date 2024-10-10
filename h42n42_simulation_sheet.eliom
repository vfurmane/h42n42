let%shared c () =
  Eliom_content.Html.F.(
    section
      ~a:[a_class ["px-6"; "py-4"; "min-w-64"; "bg-amber-50"]]
      [ article
          [ h2 ~a:[a_class ["mb-3"]] [txt "Caption"]
          ; ul
              [ li
                  ~a:[a_class ["flex"; "items-center"]]
                  [ div
                      ~a:
                        [ a_title "Purple color"
                        ; a_class
                            [ "inline-block"
                            ; "mr-1.5"
                            ; "size-4"
                            ; "bg-purple-800"
                            ; "rounded" ] ]
                      []
                  ; txt "– Healthy creet" ] ] ] ])
