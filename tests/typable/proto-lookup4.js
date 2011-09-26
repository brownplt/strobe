function foo(o, s) 
/*:   { __proto__: {y: Bool, __proto__: Object, z: _ }, y: _, z: _, x: Int} 
    * /(x|(y|z))/ 
   -> Int + Bool + Undef */ {
    return o[s];
}
