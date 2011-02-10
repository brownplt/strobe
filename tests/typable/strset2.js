function foo(reject, o, s)
/*: ((${"__proto__", "constructor"} -> True) & 
     ($^{"__proto__", "constructor"} -> Bool)) *
    {__proto__: Bool, "constructor": Int, #proto: Null, *: Str, #code: Bot} *
    Str 
  -> Str + Undef
     */
{
    if(reject(s)) {
        return "";
    }
    else {
        return o[s];
    }
}
