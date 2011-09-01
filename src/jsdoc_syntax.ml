type typ =
  | All (** [{*}] *)
  | Var of string (** [{boolean}] *)
  | App of string * typ list (** [{Object.<string, number>}] *)
  | Union of typ * typ (** [{(number|boolean)}] *)
  | Record of (string * typ) list (** [{{myNum: number, myObject}}] *)
  | Nullable of typ (** [{?number}] *)
  | NonNullable of typ (** [{!Object}] *)
  | Func of func_typ

and func_typ = {
  new_ : typ option; (** [{function(new:Object, string)}] *)
  this : typ option; (** [{function(this:Object, string)}] *)
  args : typ list; (** [{function(string, boolean)}] *)
  vararg : typ option; (** [{function(string, ...[number]): number}] *)
  result : typ option (** [{function(): number}] *)
}
