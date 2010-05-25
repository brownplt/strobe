{
  var fn = function(x) /*: Any -> Undef */ {
    var t = typeof x;
    switch (t) {
      case "number":
      case "boolean":
      case "string":
      case "undefined":
        return x;
      case "function":
        return false;
    }

   if (x === null) { return x; } else { return "OMG AN OBJECT"; }
  };
}
