function f() /*: -> Any */ {
    try {
        throw 10;
    }
    catch (e) {
        return e;
    }
}