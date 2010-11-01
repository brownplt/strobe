function foo() /*: -> Undef */ {

    var o = /*: obj* 'AdObj */ {
        fun: /*:upcast 'Ad */ (/*: obj* 'AdObj */ (function () /*: -> Undef */ { }))};

}