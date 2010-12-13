function foo(o, f) 
/*:
Num +
{     valueOf : _,
      hasOwnProperty : _,
      toString : _,
      arguments : _,
      #proto : Object + Function + Array,
      * : Num + Str,
      #code : Bool ... -> Str
     }
*
(Num + 
{     valueOf : _,
      hasOwnProperty : _,
      toString : _,
      arguments : _,
      #proto : Object + Function + Array,
      * : Num + Str,
      #code : Bool ... -> Str + Num
     } -> Bool)
-> Bool */
{
    return f(o);
}
