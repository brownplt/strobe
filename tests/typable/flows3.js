function(x) /*: Num + Undef -> Num */ {
    if (typeof x === "number") { 
        return x;
    }
    else {
        return 900;
    }
};

