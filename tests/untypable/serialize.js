function serialize(val) /*: Any ->  Str + Bool */ {
  switch (typeof val) {
  case "function": 
  case "undefined":
      return false;
  case "boolean":
      return val ? "true" : "false";
  case "string":
      return val;
  case "number":
      return "" + val;


  }
  
  if (val === null) { return "null"; } 
  
  var fields = /*: Str */ [ ];
  for (var p in val) {
    var v = serialize(val[p]);
    if (typeof v === "string") {
        fields.push(p + ": " + v);
    }
  }
  
return "{ " + fields.join(", ") + " }";

}
