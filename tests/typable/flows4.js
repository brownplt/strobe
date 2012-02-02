function(x) /*: Num + Undef -> Num */ {
    var y = 2990;
    if (typeof x === "number") { 
        y = x;
    }
    else {
        y = 900;
    }

    return y;
};
