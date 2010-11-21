function C1() /*: constructor -> {x: Int} */ {
    this.x = 5;
};


function C2() /*: constructor -> {c: C1, d: Int} */ {
    this.c = new C1();
    this.d = 0;
};

function bar() /*: -> Undef */ {

}

function foo(o) /*: C2 + C1 -> Int */ {

    if(o.x) {
        return 4;
    }

    return o.d; //can't safely assume this, since o.c can be 0
};
