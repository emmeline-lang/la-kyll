type Document a =

type Template =

let get_template = foreign "get_template" forall . String -> Document Template
