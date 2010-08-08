function foo(a,b) /*: {x:Num, #proto:Object, *:Bool + Undef} * 
                      {x:Num, #proto:Object, *:Bool} -> Undef */
{
    a.foo = b.foo; // should succeed if b <: a
}

