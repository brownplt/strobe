function foo() /*: -> Null + Array<Str> */ {
    var re = /W+/;
    return re.match("asdf");
}
