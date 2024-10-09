let%client performance_now () : float =
  let open Js_of_ocaml in
  let perf = Js.Unsafe.js_expr "performance" in
  Js.Unsafe.meth_call perf "now" [||]
