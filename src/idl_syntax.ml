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

type interfaceMember =
  | Attribute of pos * readOnly * typ * id
  | Operation of pos * typ * id * typ list
  | ConstMember of pos * typ * id


type definition =
  | Module of pos * id * definition list
  | Typedef of pos * typ * id
  | Interface of pos * id * scopedName option * interfaceMember list
  | ForwardInterface of pos * id
  | Exception of pos * id * interfaceMember list
  | Const of pos * typ * id

