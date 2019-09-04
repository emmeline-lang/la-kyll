import "std" List as L
import "la-kyll" Command as C

let list = L.Cons (C.text_rule "index.html" (C.return ())) L.Nil

let () = C.eval list
