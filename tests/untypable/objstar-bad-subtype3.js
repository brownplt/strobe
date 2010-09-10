function foo(a,b) /*: {x:Num, #proto:Object, *:Bool + Int, #code: Undef} * 
                      {x:Num, y:Bool, #proto:Object, *:Bool, #code: Undef} -> Undef */
{
    a = b;
    a.y = 5;
    return b; // now b.y isn't a bool
}

