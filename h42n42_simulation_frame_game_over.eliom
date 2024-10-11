let%shared c () =
  Eliom_content.Html.D.(
    div
      ~a:
        [ a_class
            [ "absolute"
            ; "z-20"
            ; "flex"
            ; "justify-center"
            ; "items-center"
            ; "size-full"
            ; "bg-black/20"
            ; "backdrop-blur-sm" ] ]
      [p ~a:[a_class ["text-white"]] [txt "Game Over"]])
