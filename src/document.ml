type 'a t = {
    name : string;
    yaml : (string * Yaml.value) list;
    content : 'a;
  }
