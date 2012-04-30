type comments = (Prelude.Pos.t * string) list

type typ_db

val new_decls : (Prelude.Pos.t * string) list -> Typedjs_syntax.env_decl list

val read_typs : JavaScript_syntax.prog -> comments -> typ_db

val get_annotation : typ_db -> Prelude.Pos.t -> Typedjs_syntax.annotation option
