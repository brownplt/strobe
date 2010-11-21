function C2() /*: constructor -> {c: C1 + Int} */ {
    this.c = new C1();
};

function foo(o) /*: C2 + Int -> Int */ {
    if(o.c) {
        return 5;
    }
    else {
        return o; //can't safely assume this, since o.c can be 0
    }
};
