var ADSAFE = (function () /*:  -> Any */ {

    var adsafe_lib /*: upcast Undef + 'Ad */,
        adsafe_id /*: upcast Undef + Str + Null */,

// These member names are banned from guest scripts. The ADSAFE.get and
// ADSAFE.put methods will not allow access to these properties.
        banned = 
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
                  #proto: Object, *:Bot, #code: Bot} */
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
        },

        cache_style_node /*: upcast Undef + HTMLElement */,
        cache_style_object /*: upcast Undef + 'Style */,
        defaultView = document.defaultView,
        ephemeral /*: upcast 'Ad */,
        flipflop = false,       // Used in :even/:odd processing
        has_focus /*: upcast Undef + Null + HTMLElement */,
        // Set of hunter patterns (a dictionary)
        hunter /*: upcast Undef + 
                   {#proto: Object, *: HTMLElement + Undef -> Undef, #code: Undef} */ ,
//        interceptors = [],

        makeableTagName =
        /*: obj* {a         : ${"a"},
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
              },
              
        name,
        pecker,
        result /*: upcast Undef + Array<HTMLElement + Undef> */,
        star,          
        the_range,
        value /*: upcast Undef + Str */;

    // The error function is called if there is a violation or confusion
    // Its return type is Bot because it *always* throws an exception.
    // This is useful for a typing discipline because the union of any
    // type T with a call to error() is equivalent to T.

    function error(message) /*: Str + Undef -> Bot */ {
        //log("ADsafe error: " + (message || "ADsafe violation."));
        throw {
            name: "ADsafe",
            message: (message || "ADsafe violation.")
        };
    }

    //	Some of JavaScript's implicit string conversions can grant extraordinary
    //	powers to untrusted code. So we use the string_check function to prevent
    //  such abuses.
    //  Using (return error()) rather than just (error()) is a pattern that 
    //  comes up a few times while typing ADsafe.
    
    function string_check(string) /*: Any -> Str */ {
        if (typeof string !== 'string') {
            return error("ADsafe string violation.");
        }
        return string;
    }

    //  The object.hasOwnProperty method has a number of hazards. So we wrap it in
    //  the owns function.   

    function owns(object, string) /*: Any * Any -> Bool */ {
        return object && typeof object === 'object' &&
	    /*: cheat Bool */ (Object.prototype.hasOwnProperty.call(object, string_check(string)));
    }

//  Firefox implemented some of its array methods carelessly. If a method is
//  called as a function it returns the global object. ADsafe cannot tolerate
//  that, so we wrap the methods to make them safer and slower.

    var mozilla = (/*: cheat trec f . Any -> 'f */ (function(name) /*: Any -> Any */ {
        var method = Array.prototype[name];
        Array.prototype[name] = function () /*: -> Any */ {
            return this.window ? error() : method.apply(this, /* arguments */);
        };
//        return mozilla;
    }))
    ('concat')
    ('every')
    ('filter')
    ('forEach')
    ('map')
    ('reduce')
    ('reduceRight')
    ('reverse')
    ('slice')
    ('some')
    ('sort');

    var reject_name = 
        /*: cheat (('banned -> True) + ('not_banned -> False)) */
    (function (name) /*: 'Ad -> Bool */ {
        return 
        ((typeof name !== 'number' || name < 0) &&
         (typeof name !== 'string' || name.charAt(0) === '_' ||
          name.slice(-1) === '_' || name.charAt(0) === '-'))
            || banned[name];
    });

    function reject_property(object, name) /*: 'Ad * 'Ad -> Bool */ {
        if(reject_name(name)) {
            return true;
        }
        return typeof object !== 'object';
    }

    // The getStyleObject function returns the computed style object for a node.
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

//    function F() {}

//  Return the ADSAFE object.

    return /*: obj* 'AdObj */ {

//        create: typeof Object.create === 'function' ? Object.create : function (o) {
//            F.prototype = typeof o === 'object' && o ? o : Object.prototype;
//            return new F();
//        },

//  ADSAFE.get retrieves a value from an object.

        get: 
        /*: upcast 'Ad */
        (/*: obj* 'AdObj */ (function(object, name) 
            /*: ['Ad + HTMLWindow] 'Ad * 'Ad * 'Ad ... -> 'Ad */ 
            {
                if (typeof object !== "object") { return error(); }
                if (!reject_name(name)) {
                    return object[name];
                }
                return error();
            })),


//  ADSAFE.go allows a guest widget to get access to a wrapped dom node and
//  approved ADsafe libraries. It is passed an id and a function. The function
//  will be passed the wrapped dom node and an object containing the libraries.

//        go: ADSAFE_go,

//  ADSAFE.id allows a guest widget to indicate that it wants to load
//  ADsafe approved libraries.

//        id: function (id) {

//  Calls to ADSAFE.id must be balanced with calls to ADSAFE.go.
//  Only one id can be active at a time.

//            if (adsafe_id) {
//                return error();
//            }
//            adsafe_id = id;
//            adsafe_lib = {};
//        },

//  ADSAFE.isArray returns true if the operand is an array.

//        isArray: Array.isArray || function (value) {
//            return Object.prototype.toString.apply(value) === '[object Array]';
//        },

//        later: later,

//  ADSAFE.lib allows an approved ADsafe library to make itself available
//  to a widget. The library provides a name and a function. The result of
//  calling that function will be made available to the widget via the name.

//        lib: lib,

//  ADSAFE.log is a debugging aid that spams text to the browser's log.

//        log: log,

//        remove: ADSAFE_remove,

//        set: ADSAFE_set,

//  ADSAFE._intercept allows the page to register a function that will
//  see the widget's capabilities.

//        _intercept: function (f) {
//            interceptors.push(f);
//        }

    };
}());