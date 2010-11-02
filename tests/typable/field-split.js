function foo(o) /*: Date + Int -> Int */ {
    if(o.getDate) {
        return 5;
    }
    else {return o;}
}
