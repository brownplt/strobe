function f() /*: -> Array<Str + Undef> + Undef */ {
    var rg = /s/;
    return rg.exec("foozle");
}