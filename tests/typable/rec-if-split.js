var func  = function(o) 
/*: (trec X . Int + {left: Int, right: 'X, toString: -> Str}) -> Undef */ {

  var d = 0;

  if(typeof o === "object") {
    d = o.left + 6;
  }
  else {
    d = o + 7;
  }

};