open Prelude
open Typedjs_syntax

let init_env =
  IdMapExt.from_list
    [ ("document", TDom); ("window", TDom); ("setTimeout", TDom) ]
