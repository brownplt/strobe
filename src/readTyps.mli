type comments = (Prelude.pos * string) list

type typ_db

val new_decls : (Prelude.pos * string) list -> Typedjs_syntax.env_decl list

val read_typs : JavaScript_syntax.prog -> comments -> typ_db

val get_annotation : typ_db -> Prelude.pos -> Typedjs_syntax.annotation option
