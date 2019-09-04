import "std" List as L
import "la-kyll" Command as C

let site_template = C.load_template "site.html"

let list =
  L.Cons (C.text_rule "index.html" (C.apply_template site_template)) L.Nil

let () = C.eval list
