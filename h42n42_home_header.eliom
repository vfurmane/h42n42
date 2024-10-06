let%shared c ~cta_service () =
  Eliom_content.Html.F.(
    div
      ~a:[a_class ["main-title"]]
      [ p ~a:[a_class ["name"]] [txt "h42n42"]
      ; h1
          ~a:[a_class ["heading"]]
          [ txt
              "Simulate the life of a population of creatures
threatened by a terrifying virus"
          ]
      ; Hnds_link_button.c ~service:cta_service [txt "Simulate now"] () ])
