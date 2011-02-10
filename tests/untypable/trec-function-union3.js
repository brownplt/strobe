function foo(val1)
/*: (trec t . {y: Bool, x: 't, toString : -> Str} + ('t -> Bool))
-> Bool */ 
{
    if(typeof val1 === "function") {
        return val1({y:true, 
                     x: function(v) 
                     /*: (trec t . {y: Bool, x: 't, toString : -> Str} + ('t -> Bool)) -> Bool */  
                     { return true; }});
    }
    return false;
}
