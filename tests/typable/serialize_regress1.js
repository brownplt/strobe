function serialize(val) /*: Any ->  Bool */ {
  switch (typeof val) {
  case "function": 
      return true;
      
  case "boolean":
      return val;


  }
  return false;

}
