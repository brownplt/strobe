function foo(a) /*: (trec t . Array<'t> + Int) -> Int */ {
    var res = /*: upcast (trec t . Array<'t> + Int) */ 0, i=0;

    if(a instanceof Array) {
        res = a[0];
        if(typeof res === "number") {
            return res;
        }
        return foo(res);
    }
    else {
        return a;
    }

}
