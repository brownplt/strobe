function serialize(val) /*: Str + Bool + Num ->  Bool */ {
  switch (typeof val) {
  case "string":
  case "number":
      return true;
  }
  return val ;
}
