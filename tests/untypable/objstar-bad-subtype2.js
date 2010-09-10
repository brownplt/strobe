function foo(a,b) /*: {x:Num, #proto:Object, *:Bool + Undef, #code: Undef} * 
                      {x:Num, y:Str, #proto:Object, *:Bool, #code: Undef} -> (Bool + Undef) */
{
    a.y = b.y; // should fail if b <: a
    return a.y;
}

