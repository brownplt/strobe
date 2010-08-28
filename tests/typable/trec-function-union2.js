function foo(val1, val2)
/*: (trec t . {y: Bool, x: 't} + ('t -> Bool)) *
(trec t . {y: Bool, x: 't} + ('t -> Bool))
-> Bool */ 
{
    if(typeof val1 === "function") {
        return val1(val2);
    }
    return false;
}
