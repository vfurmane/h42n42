(** [link_button ~a ~service children ()] is a DOM element representing an anchor tag to another service, styled as a button using the the ["hnds-button"] class.*)
let%shared c ?a:attributes ~service children =
  let attributes =
    Eliom_content.Html.D.a_class ["hnds-button"]
    (* TODO clsx like function instead *)
    :: Option.value attributes ~default:[]
  in
  Eliom_content.Html.D.(a ~service ~a:attributes children)
