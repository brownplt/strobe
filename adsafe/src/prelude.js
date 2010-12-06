
var defaultView = document.defaultView,
cache_style_node /*: upcast Undef + HTMLElement */,
cache_style_object /*: upcast Undef + 'Style */,
has_focus /*: upcast Undef + Null + HTMLElement */,
value /*: upcast Undef + Str */,
adsafe_lib /*: upcast Undef + 'Ad */,
adsafe_id /*: upcast Undef + Str + Null */ ;

var banned = 
    /*: obj* {arguments     : Bool,
              callee        : Bool,
              caller        : Bool,
              "constructor" : Bool,
              "eval"        : Bool,
              "prototype"   : Bool,
              stack         : Bool,
              unwatch       : Bool,
              valueOf       : Bool,
              watch         : Bool,
              #proto: Object, *:Bool, #code: Bot} */
{
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



var makeableTagName =
/*: obj* {
    a         : ${"a"},
    abbr      : ${"abbr"},
    acronym   : ${"acronym"},
    address   : ${"address"},
    area      : ${"area"},
    b         : ${"b"},
    bdo       : ${"bdo"},
    big       : ${"big"},
    blockquote: ${"blockquote"},
    br        : ${"br"},
    button    : ${"button"},
    canvas    : ${"canvas"},
    caption   : ${"caption"},
    center    : ${"center"},
    cite      : ${"cite"},
    code      : ${"code"},
    col       : ${"col"},
    colgroup  : ${"colgroup"},
    dd        : ${"dd"},
    del       : ${"del"},
    dfn       : ${"dfn"},
    dir       : ${"dir"},
    div       : ${"div"},
    dl        : ${"dl"},
    dt        : ${"dt"},
    em        : ${"em"},
    fieldset  : ${"fieldset"},
    font      : ${"font"},
    form      : ${"form"},
    h1        : ${"h1"},
    h2        : ${"h2"},
    h3        : ${"h3"},
    h4        : ${"h4"},
    h5        : ${"h5"},
    h6        : ${"h6"},
    hr        : ${"hr"},
    i         : ${"i"},
    img       : ${"img"},
    input     : ${"input"},
    ins       : ${"ins"},
    kbd       : ${"kbd"},
    label     : ${"label"},
    legend    : ${"legend"},
    li        : ${"li"},
    map       : ${"map"},
    menu      : ${"menu"},
    object    : ${"object"},
    ol        : ${"ol"},
    optgroup  : ${"optgroup"},
    option    : ${"option"},
    p         : ${"p"},
    pre       : ${"pre"},
    q         : ${"q"},
    samp      : ${"samp"},
    select    : ${"select"},
    small     : ${"small"},
    span      : ${"span"},
    strong    : ${"strong"},
    sub       : ${"sub"},
    sup       : ${"sup"},
    table     : ${"table"},
    tbody     : ${"tbody"},
    td        : ${"td"},
    textarea  : ${"textarea"},
    tfoot     : ${"tfoot"},
    th        : ${"th"},
    thead     : ${"thead"},
    tr        : ${"tr"},
    tt        : ${"tt"},
    u         : ${"u"},
    ul        : ${"ul"},
    "var"     : ${"var"},
    #proto    : Object,
    *         : Bot,
    #code     : Bot
} */
{
    a         : "a",
    abbr      : "abbr",
    acronym   : "acronym",
    address   : "address",
    area      : "area",
    b         : "b",
    bdo       : "bdo",
    big       : "big",
    blockquote: "blockquote",
    br        : "br",
    button    : "button",
    canvas    : "canvas",
    caption   : "caption",
    center    : "center",
    cite      : "cite",
    code      : "code",
    col       : "col",
    colgroup  : "colgroup",
    dd        : "dd",
    del       : "del",
    dfn       : "dfn",
    dir       : "dir",
    div       : "div",
    dl        : "dl",
    dt        : "dt",
    em        : "em",
    fieldset  : "fieldset",
    font      : "font",
    form      : "form",
    h1        : "h1",
    h2        : "h2",
    h3        : "h3",
    h4        : "h4",
    h5        : "h5",
    h6        : "h6",
    hr        : "hr",
    i         : "i",
    img       : "img",
    input     : "input",
    ins       : "ins",
    kbd       : "kbd",
    label     : "label",
    legend    : "legend",
    li        : "li",
    map       : "map",
    menu      : "menu",
    object    : "object",
    ol        : "ol",
    optgroup  : "optgroup",
    option    : "option",
    p         : "p",
    pre       : "pre",
    q         : "q",
    samp      : "samp",
    select    : "select",
    small     : "small",
    span      : "span",
    strong    : "strong",
    sub       : "sub",
    sup       : "sup",
    table     : "table",
    tbody     : "tbody",
    td        : "td",
    textarea  : "textarea",
    tfoot     : "tfoot",
    th        : "th",
    thead     : "thead",
    tr        : "tr",
    tt        : "tt",
    u         : "u",
    ul        : "ul",
    'var'     : "var"
};

//function reject__name(name) /*: (('banned -> True) + ('not_banned -> False)) */ {
//    return 
//    ((typeof name !== 'number' || name < 0) &&
//     (typeof name !== 'string' || name.charAt(0) === '_' ||
//      name.slice(-1) === '_' || name.charAt(0) === '-'))
//        || /*: cheat 'Ad */ (banned[name]);
//}

function safe_name(name) /*: 'Ad -> 'not_banned */ {
    if(reject_name(name)) {
        return error("ADsafe string violation");
    }
    else {
        return name;
    }
}

function log(s) /*: Str -> Undef */ {
    if (window.console) {
        console.log(s);        /* Firebug */
    } else if (typeof Debug === 'object') {
        Debug.writeln(s);      /* IE */
    }
}

function error(message) /*: Str + Undef -> Bot */ {
    log("ADsafe error: " + (message || "ADsafe violation."));
    throw {
        name: "ADsafe",
        message: (message || "ADsafe violation.")
    };
}


function walkTheDOM(node, func, skip) 
/*: HTMLElement + Undef * (HTMLElement + Undef -> Undef) * Bool + Undef -> Undef */ {
    
    // Recursively traverse the DOM tree, starting with the node, in document
    // source order, calling the func on each node visisted.
    
    if (!skip) {
        func(node);
    }
    node = node.firstChild;
    while (node) {
        walkTheDOM(node, func);
        node = node.nextSibling;
    }
}

function purge_event_handlers(node) 
/*: HTMLElement + Undef -> Undef */ {

// We attach all event handlers to a ___ on ___ property. The property name
// contains spaces to insure that there is no collision with HTML attribues.
// Keeping the handlers in a single property makes it easy to remove them
// all at once. Removal is required to avoid memory leakage on IE6 and IE7.

    walkTheDOM(node, 
               function (node) /*: HTMLElement + Undef -> Undef */ {
                   if (node.tagName) {
                       node['___ on ___'] = node.change = undefined;
                   }
               }
              );
}

function getStyleObject(node) /*: HTMLElement + Undef -> 'Style + Undef */
{
    
    // The getStyleObject function returns the computed style object for a node.
    
    if (node === cache_style_node) {
        return cache_style_object;
    }
    cache_style_node = node;
    cache_style_object =
        node.currentStyle || defaultView.getComputedStyle(node, '');
    return cache_style_object;
}

function lib (name, f) 
/*: 'Ad * 'Ad -> 'Ad */
{
    if (reject_name(name)) {
	return error("ADsafe lib violation.");
    }
    adsafe_lib[name] = f(adsafe_lib);
}


//  ADSAFE.later calls a function at a later time.

function later (func, timeout) 
/*: 'Ad * 'Ad -> 'Ad */
{
    if (typeof func === 'function') {
        setTimeout(func, timeout || 0);
    } else {
        return error();
    }
}

function reject_global(that) 
/*: HTMLWindow + 'Ad -> Undef */
{
    if (that.window) {
	error();
    }
}

//	Some of JavaScript's implicit string conversions can grant extraordinary
//	powers to untrusted code. So we use the string_check function to prevent
//  such abuses.

function string_check(string) 
/*: Any -> Str */
{
    if (typeof string !== 'string') {
        return error("ADsafe string violation.");
    }
    return string;
}


function owns(object, string) /*: Any * Any -> Bool */ {
    return object && typeof object === 'object' &&
	/*: cheat Bool */ (Object.prototype.hasOwnProperty.call(object, string_check(string)));
}


function getOwnProperty() /*: -> Undef */ {
}
