function foo(a) /*: (trec t . Array<'t> + Int) -> Int */ {
    var acc = 0, i=0;

    if(a instanceof Array) {
        for(i = 0; i < a.length; i++) {
            acc += foo(a[i]);
        }
        return acc;
    }
    else {
        return a;
    }

}
