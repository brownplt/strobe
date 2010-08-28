function foo(val) 
/*: (trec t . Int + 't -> Bool) -> Bool */ 
{
    if(typeof val === "function") {
        return val(5);
    }
    return false;
}
