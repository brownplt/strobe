
var banned = {
    'arguments'     : true,
    callee          : true,
    caller          : true,
    constructor     : true,
    'eval'          : true,
    prototype       : true,
    stack	    : true,
    unwatch         : true,
    valueOf         : true,
    watch           : true
};


var makeableTagName = {

// This is the whitelist of elements that may be created with the .tag(tagName)
// method.

    a         : true,
    abbr      : true,
    acronym   : true,
    address   : true,
    area      : true,
    b         : true,
    bdo       : true,
    big       : true,
    blockquote: true,
    br        : true,
    button    : true,
    canvas    : true,
    caption   : true,
    center    : true,
    cite      : true,
    code      : true,
    col       : true,
    colgroup  : true,
    dd        : true,
    del       : true,
    dfn       : true,
    dir       : true,
    div       : true,
    dl        : true,
    dt        : true,
    em        : true,
    fieldset  : true,
    font      : true,
    form      : true,
    h1        : true,
    h2        : true,
    h3        : true,
    h4        : true,
    h5        : true,
    h6        : true,
    hr        : true,
    i         : true,
    img       : true,
    input     : true,
    ins       : true,
    kbd       : true,
    label     : true,
    legend    : true,
    li        : true,
    map       : true,
    menu      : true,
    object    : true,
    ol        : true,
    optgroup  : true,
    option    : true,
    p         : true,
    pre       : true,
    q         : true,
    samp      : true,
    select    : true,
    small     : true,
    span      : true,
    strong    : true,
    sub       : true,
    sup       : true,
    table     : true,
    tbody     : true,
    td        : true,
    textarea  : true,
    tfoot     : true,
    th        : true,
    thead     : true,
    tr        : true,
    tt        : true,
    u         : true,
    ul        : true,
    'var'     : true
};

function log(s) /*: Str -> Undef */ {
    return /*: cheat Undef*/undefined;
    // if (window.console) {
    //     console.log(s);        /* Firebug */
    // } else if (typeof Debug === 'object') {
    //     Debug.writeln(s);      /* IE */
    // }
}

function error(message) /*: Str + Undef -> Undef */ {
    log("ADsafe error: " + /*: cheat Str */(message || "ADsafe violation."));
    throw {
        name: "ADsafe",
        message: /*: cheat Str */(message || "ADsafe violation.")
    };
}


    function walkTheDOM(node, func, skip) 
    /*: Node * (Node -> Undef) * Bool + Undef -> Undef */ {

// Recursively traverse the DOM tree, starting with the node, in document
// source order, calling the func on each node visisted.

        if (/*:cheat Bool */(!skip)) {
            func(node);
        }
        node = /*:cheat Node */(node.firstChild);
        while (/*: cheat Bool */node) {
            walkTheDOM(node, func);
            node = /*: cheat Node*/(node.nextSibling);
        }
    }
