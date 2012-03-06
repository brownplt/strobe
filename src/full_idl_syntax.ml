type id = Id.t
type pos = Lexing.position


type readOnly = 
  | ReadOnly
  | NoReadOnly

type unsigned =
  | Unsigned
  | NoUnsigned

type isCallback =
  | IsCallbackInterface
  | IsNormalInterface

type stringifier =
  | IsStringifier
  | IsNormal

type paramDirection =
  | InParam
  | OutParam
  | InOutParam

type variadic =
  | Single
  | Variadic

type scopedName = 
  | RelativeName of id list
  | AbsoluteName of id list

type typ =
  | Short of unsigned
  | Long of unsigned
  | LongLong of unsigned
  | Boolean
  | Byte
  | Octet
  | Float
  | Double
  | DOMString
  | Date 
  | Any
  | Object
  | Name of scopedName
  | Void
  | Array of typ
  | Ques of typ
  | Sequence of typ
  | Native of string

type expr =
  | BinOp of expr * binop * expr
  | UnOp of unop * expr
  | Ident of scopedName
  | IntLit of int64
  | FloatLit of float
  | String of string
  | Bool of bool
and binop =
  | Plus | Minus | Times | Divide | Mod | ShLeft | ShRight | And | Xor | Or
and unop =
  | UPlus | UMinus | UTilde

type argument = paramDirection * meta list * typ * variadic * id * expr option

and meta =
  | NoInterfaceObject
  | OverrideBuiltins
  | PutForwards of id
  | NamedConstructor of id * argument list
  | Constructor of argument list
  | SizeOf of id
  | ReplaceableNamedProperties
  | Unforgeable
  | Replaceable
  | MCallback
  | MCallbackFunctionOnly
  | TreatNullAs of typ
  | AllowAny
  | Clamp
  | Scriptable
  | NotXPCOM
  | NoScript
  | PrivateBrowsingCheck
  | Unsafe
  | Retval
  | Optional
  | OptionalArgc
  | Uuid of string
  | ImplicitJSContext
  | AttrNoArgs of id
  | AttrArgList of id * expr list
  | AttrNamedArgList of id * id * expr list
  | AttrNamedIdent of id * id

type qualifiers = { static : bool;
                    getter : bool;
                    setter : bool;
                    creator : bool;
                    deleter : bool;
                    legacyCaller : bool }

type interfaceMember =
  | Attribute of pos * meta list * readOnly * stringifier * typ * id
  | Operation of pos * meta list * stringifier * qualifiers * typ * id option * argument list
  | ConstMember of pos * meta list * typ * id * expr
  | Stringifier of pos * meta list


type definition =
  | Module of pos * meta list * id * definition list
  | Typedef of pos * meta list * typ * id
  | Interface of pos * meta list * id * scopedName option * interfaceMember list * isCallback
  | ForwardInterface of pos * meta list * id
  | Exception of pos * meta list * id * scopedName option * interfaceMember list
  | Implements of pos * meta list * id * id
  | Const of pos * meta list * typ * id * expr
  | Dictionary of pos * meta list * id * scopedName option * interfaceMember list
  | PartialInterface of pos * meta list * id * interfaceMember list
  (* NON-STANDARD DEFINITIONS *)
  | Include of pos * meta list * string
  | Callback of pos * meta list * id * argument list * typ
  | Enum of pos * meta list * id * string list
