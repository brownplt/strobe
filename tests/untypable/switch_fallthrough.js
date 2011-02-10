function serialize(val) /*: Str + Bool ->  Bool */ {
  switch (typeof val) {
  case "string": 
      return true;
  }
  return val;

}
