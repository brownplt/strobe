
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



var makeableTagName = /*: obj* {#proto: Object, *: Bool, #code: Bot} */
{

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

function reject_lookup(obj, name) /*: 'Ad * 'Ad -> 'Ad */ {
    return /*: cheat 'Ad */ (banned[name] || 
                             ((typeof name !== 'number' || name < 0) &&
                              (typeof name !== 'string' || name.charAt(0) === '_' ||
                               name.slice(-1) === '_'   || name.charAt(0) === '-')) ?
                             error() :
                             obj[name]);
}

function reject_mutate(obj, name, value) /*: 'Ad * 'Ad * 'Ad -> 'Ad */ {
    return /*: cheat 'Ad */ (banned[name] || 
                             ((typeof name !== 'number' || name < 0) &&
                              (typeof name !== 'string' || name.charAt(0) === '_' ||
                               name.slice(-1) === '_'   || name.charAt(0) === '-')) ?
                             error() :
                             obj[name] = value);
}

function make_safe_tag(tag) /*: Str -> HTMLElement + Undef */ {
    if(makeableTagName[tag] === true) {
        return document.createElement(tag);
    }
    else {
        return error();
    }
}



function reject__name(name) /*: 'Ad -> Any */ {
    return 
    ((typeof name !== 'number' || name < 0) &&
     (typeof name !== 'string' || (/*:cheat Str */name).charAt(0) === '_' ||
      /*:cheat Str*/name.slice(-1) === '_' || /*: cheat Str*/name.charAt(0) === '-'))
        || /*: cheat 'Ad */ (banned[name]);
}

function log(s) /*: Str -> Undef */ {
    return /*: cheat Undef*/undefined;
    // if (window.console) {
    //     console.log(s);        /* Firebug */
    // } else if (typeof Debug === 'object') {
    //     Debug.writeln(s);      /* IE */
    // }
}

function error(message) /*: Str + Undef -> Undef */ {
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

// JSlint ensures that calls to lib have a string here
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
    if (typeof string === 'string') {
	error("ADsafe string violation.");
    }
    return /*: cheat Str */ string;
}


function owns(object, string) /*: Any * Any -> Bool */ {
    return object && typeof object === 'object' &&
	/*:cheat Bool */ (Object.prototype.hasOwnProperty.call(object, string_check(string)));
}


function getOwnProperty() /*: -> Undef */ {
}
