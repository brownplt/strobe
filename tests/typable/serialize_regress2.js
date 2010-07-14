function serialize(val) /*: Any ->  Str + Bool */ {
  switch (typeof val) {
  case "function": 
  case "undefined":
      return false;
  case "boolean":
      return val;
  }
  return false;
}
