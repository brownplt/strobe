function foo(o, s) 
/*:   {proto: {y: Bool}; x: Int} 
    * /(x|(y|z))/ 
   -> Int + Bool + Undef */ {
    return o[s];
}
