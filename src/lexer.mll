{
}

rule main = parse
  | "---\n"
    { Lexing.new_line lexbuf;
      let yaml = yaml (Buffer.create 100) lexbuf in
      content (Some yaml) lexbuf }
  | "" { content None lexbuf }

and yaml buf = parse
  | "---\n" { Lexing.new_line lexbuf; Buffer.contents buf }
  | [^ '\n']* '\n'
    { Lexing.new_line lexbuf;
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      yaml buf lexbuf }

and content yaml = parse
  _* eof { (yaml, Lexing.lexeme lexbuf) }
