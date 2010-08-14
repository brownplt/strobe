function simple(val) /*: Ad -> Ad */ {
    return val.foo;
}

function ths(val) /*: [Ad] Ad -> Ad */ {
    var b = this.something;
    return b;
}
