function foo(reject, o, s)
/*: ((${"__proto__", "constructor"} -> True) + 
     ($^{"__proto__", "constructor"} -> False)) *
    {__proto__: Bool, "constructor": Int, #proto: Null, *: Str, #code: Bot} *
    Str 
  -> Str + Undef
     */
{
    if(!reject(s)) {
        return o[s];
    }
    else {
        return "";
    }
}
