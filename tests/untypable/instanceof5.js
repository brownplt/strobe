function foo(a) /*: (Array<Int> + Int) -> Int */ {
    var acc = 0, i=0;

    if(a instanceof Array) {
        for(i = 0; i < a.length; i++) {
            acc += a[i];
        }
        return acc;
    }
    else {
        return a;
    }

}
