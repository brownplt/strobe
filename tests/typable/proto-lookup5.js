function foo(o, s) 
/*:   {proto: {y: Bool}, *: Str; x: Int} 
    * /(x|(y|z))/ 
   -> Int + Bool + Str + Undef */ {
    return o[s];
}
