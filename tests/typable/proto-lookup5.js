function foo(o, s) 
/*:   { __proto__: {y: Bool, __proto__: Object, *: _}, *: Str, x: Int } 
    * /(x|(y|z))/ 
   -> Int + Bool + Str + Undef */ {
    return o[s];
}
