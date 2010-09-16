function foo(a,b) /*: {x:Num, #proto:Object, *:Bool + Undef, #code: Undef} * 
                      {x:Num, #proto:Object, *:Bool, #code: Undef} -> Undef */
{
    a.foo = b.foo; // should succeed if b <: a
}

