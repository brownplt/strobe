type id = Id.t
type pos = Lexing.position


type readOnly = 
  | ReadOnly
  | NoReadOnly

type unsigned =
  | Unsigned
  | NoUnsigned

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

type meta =
  | NoInterfaceObject
  | OverrideBuiltins
  | PutForwards of id
  | NamedConstructor of id * typ list
  | Constructor of typ list
  | ReplaceableNamedProperties
  | Unforgeable
  | Replaceable
  | Callback
  | CallbackFunctionOnly
  | TreatNullAs of typ
  | AllowAny
  | Clamp

type interfaceMember =
  | Attribute of pos * readOnly * typ * id
  | Operation of pos * typ * id * typ list
  | ConstMember of pos * typ * id
  | Creator of pos * typ * typ list
  | Getter of pos * typ * typ list
  | Setter of pos * typ * typ list
  | Deletor of pos * typ * typ list


type definition =
  | Module of pos * id * definition list
  | Typedef of pos * typ * id
  | Interface of pos * id * scopedName option * interfaceMember list * meta list
  | ForwardInterface of pos * id
  | Exception of pos * id * interfaceMember list
  | Implements of pos * id * id
  | Const of pos * typ * id
  | Dictionary of pos * id * interfaceMember list
  | PartialInterface of pos * id * interfaceMember list

