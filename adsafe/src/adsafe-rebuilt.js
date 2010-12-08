function Bunch(nodes) 
/*: constructor (Undef + Array<HTMLElement + Undef> -> {___nodes___: Array<HTMLElement + Undef> + Undef, ___star___: Bool + Undef}) */ 
{
    this.___nodes___ = nodes;
    this.___star___ = true;//star && nodes.length > 1;
    //            star = false;
}


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
    
    name /*: upcast Undef + Str */,
    pecker /*: upcast Undef + 'Pecker */, 
    result /*: upcast Undef + Array<HTMLElement + Undef> */,
    star = false, // Init so the type checker knows it's a bool
    the_range /*: upcast Undef + Range */ ,
    value /*: upcast Undef + Str */;

    // The error function is called if there is a violation or confusion
    // Its return type is Bot because it *always* throws an exception.
    // This is useful for a typing discipline because the union of any
    // type T with a call to error() is equivalent to T.

    function error(message) /*: Str + Undef -> Bot */ {
        //        ADSAFE.log("ADsafe error: " + (message || "ADsafe violation."));
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

    // Joe added this for convenience in some places, to ensure that
    // things are not on the prototype.  Used in quest, for instance
    function safe_name(name) /*: 'Ad -> 'not_banned */ {
        if(reject_name(name)) {
            return error("ADsafe string violation");
        }
        else {
            return name;
        }
    }

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

    function parse_query(text_in, id) 
    /*: 'Ad * Str + Undef -> Array<'Selector> + Undef */
    {

        // Convert a query string into an array of op/name/value selectors.
        // A query string is a sequence of triples wrapped in brackets; or names,
        // possibly prefixed by # . & > _, or :option, or * or /. A triple is a name,
        // and operator (one of [=, [!=, [*=, [~=, [|=, [$=, or [^=) and a value.

        // If the id parameter is supplied, then the name following # must have the
        // id as a prefix and must match the ADsafe rule for id: being all uppercase
        // letters and digits with one underbar.

        // A name must be all lower case and may contain digits, -, or _.

        var match /*: upcast Undef + Array<Str + Undef> */,          // A match array
        query = /*: 'Selector */ [], // The resulting query array
        selector /*: upcast Undef + 'Selector */,
        qx = id ?
            /^\s*(?:([\*\/])|\[\s*([a-z][0-9a-z_\-]*)\s*(?:([!*~|$\^]?\=)\s*([0-9A-Za-z_\-*%&;.\/:!]+)\s*)?\]|#\s*([A-Z]+_[A-Z0-9]+)|:\s*([a-z]+)|([.&_>\+]?)\s*([a-z][0-9a-z\-]*))\s*/ :
            /^\s*(?:([\*\/])|\[\s*([a-z][0-9a-z_\-]*)\s*(?:([!*~|$\^]?\=)\s*([0-9A-Za-z_\-*%&;.\/:!]+)\s*)?\]|#\s*([\-A-Za-z0-9_]+)|:\s*([a-z]+)|([.&_>\+]?)\s*([a-z][0-9a-z\-]*))\s*/,
        // Added by Joe.  Adding a new variable helps satisfy the type checker,
        // which has trouble keeping track of just doing string_check(text).
        text = string_check(text_in); 

        // Loop over all of the selectors in the text.

        do {

            // The qx teases the components of one selector out of the text, ignoring
            // whitespace.

            //          match[0]  the whole selector
            //          match[1]  * /
            //          match[2]  attribute name
            //          match[3]  = != *= ~= |= $= ^=
            //          match[4]  attribute value
            //          match[5]  # id
            //          match[6]  : option
            //          match[7]  . & _ > +
            //          match[8]      name

            match = qx.exec(text);
            if (!match) {
                return error("ADsafe: Bad query:" + text);
            }

            // Make a selector object and stuff it in the query.

            if (match[1]) {

                // The selector is * or /

                selector = /*: obj* 'Selector */ {
                    op: match[1]
                };
            } else if (match[2]) {

                // The selector is in brackets.

                selector = match[3] ? /*: obj* 'Selector */ {
                    op: /*: upcast Str + Undef */ ('[' + match[3]),
                    name: match[2],
                    value: match[4]
                } : /*: obj* 'Selector */ {
                    op: /*: upcast Str + Undef */ '[',
                    name: match[2]
                };
            } else if (match[5]) {

                // The selector is an id.

                if (query.length > 0 || match[5].length <= id.length ||
                    match[5].slice(0, id.length) !== id) {
                    return error("ADsafe: Bad query: " + text);
                }
                selector = /*: obj* 'Selector */ {
                    op: /*: upcast Str + Undef */ '#',
                    name: match[5]
                };

                // The selector is a colon.

            } else if (match[6]) {
                selector = /*: obj* 'Selector */ {
                    op: /*: upcast Str + Undef */ (':' + match[6])
                };

                // The selector is one of > + . & _ or a naked tag name

            } else {
                selector = /*: obj* 'Selector */ {
                    op: match[7],
                    name: match[8]
                };
            }

            // Add the selector to the query.

            query.push(selector);

            // Remove the selector from the text. If there is more text, have another go.

            text = text.slice(match[0].length);
        } while (text);
        return query;
    }

    hunter = 
        /*: obj* {#proto: Object, *: HTMLElement + Undef -> Undef, #code: Undef} */ 
    {
        
        // These functions implement the hunter behaviors.
        
        '': function (node) /*: HTMLElement + Undef -> Undef */ {
            var e = node.getElementsByTagName(name);
            for (var i = 0; i < 1000; i += 1) {
                // Why bound at 1000?
                if (e[i]) {
                    result.push(e[i]);
                } else {
                    break;
                }
            }
        },
        
        '+': function (node) /*: HTMLElement + Undef -> Undef */ {
            node = node.nextSibling;
            name = name.toUpperCase();
            while (node && !node.tagName) {
                node = node.nextSibling;
            }
            if (node && node.tagName === name) {
                result.push(node);
            }
        },

        '>': function (node) /*: HTMLElement + Undef -> Undef */ {
            node = node.firstChild;
            name = name.toUpperCase();
            while (node) {
                if (node.tagName === name) {
                    result.push(node);
                }
                node = node.nextSibling;
            }
        },

        '#': function (node) /*: HTMLElement + Undef -> Undef */ {
            var n = document.getElementById(name);
            if (n.tagName) {
                result.push(n); // problem with returning Null
            }
        },
        '/': function (node) /*: HTMLElement + Undef -> Undef */{
            var e = node.childNodes;
            for (var i = 0; i < e.length; i += 1) {
                result.push(e[i]);
            }
        },

        '*': function (node) /*: HTMLElement + Undef -> Undef */{
            star = true;
            walkTheDOM(node, function (node) /*: HTMLElement + Undef -> Undef */ {
                result.push(node);
            }, true);
        }
    };

    // The Pecker type is in adsafe.env
    pecker = /*: obj* 'Pecker */
    {
        '.': function (node) /*: HTMLElement + Undef -> Bool */ {
            return (' ' + node.className + ' ').indexOf(' ' + name + ' ') >= 0;
        },
        '&': function (node) /*: HTMLElement + Undef -> Bool */ {
            return node.name === name;
        },
        '_': function (node) /*: HTMLElement + Undef -> Bool */ {
            return node.type === name;
        },
        '[': function (node) /*: HTMLElement + Undef -> Bool */  {
            return typeof node[name] === 'string';
        },
        '[=': function (node) /*: HTMLElement + Undef -> Bool */  {
            var member = node[name];
            return typeof member === 'string' && member === value;
        },
        '[!=': function (node) /*: HTMLElement + Undef -> Bool */  {
            var member = node[name];
            return typeof member === 'string' && member !== value;
        },
        '[^=': function (node) /*: HTMLElement + Undef -> Bool */  {
            var member = node[name];
            return typeof member === 'string' &&
                member.slice(0, member.length) === value;
        },
        '[$=': function (node) /*: HTMLElement + Undef -> Bool */  {
            var member = node[name];
            return typeof member === 'string' &&
                member.slice(-member.length) === value;
        },
        '[*=': function (node) /*: HTMLElement + Undef -> Bool */  {
            var member = node[name];
            return typeof member === 'string' &&
                member.indexOf(String(value)) >= 0;
        },
        '[~=': function (node) /*: HTMLElement + Undef -> Bool */  {
            var member = node[name];
            return typeof member === 'string' &&
                (' ' + member + ' ').indexOf(' ' + value + ' ') >= 0;
        },
        '[|=': function (node) /*: HTMLElement + Undef -> Bool */  {
            var member = node[name];
            return typeof member === 'string' &&
                ('-' + member + '-').indexOf('-' + value + '-') >= 0;
        },
        ':blur': function (node) /*: HTMLElement + Undef -> Bool */  {
            return node !== has_focus;
        },
        ':checked': function (node) /*: HTMLElement + Undef -> Bool */  {
            return node.checked;
        },
        ':disabled': function (node) /*: HTMLElement + Undef -> Bool */  {
            return node.tagName && node.disabled;
        },
        ':enabled': function (node) /*: HTMLElement + Undef -> Bool */  {
            return node.tagName && !node.disabled;
        },
        ':even': function (node) /*: HTMLElement + Undef -> Bool */  {
            var f = false; // added initialization
            if (node.tagName) {
                f = flipflop;
                flipflop = !flipflop;
                return f;
            } else {
                return false;
            }
        },
        ':focus': function (node) /*: HTMLElement + Undef -> Bool */  {
            return node === has_focus;
        },
        ':hidden': function (node) /*: HTMLElement + Undef -> Bool */  {
            return node.tagName && 
                getStyleObject(node).visibility !== 'visible';
        },
        ':odd': function (node) /*: HTMLElement + Undef -> Bool */  {
            if (node.tagName) {
                flipflop = !flipflop;
                return flipflop;
            } else {
                return false;
            }
        },
        ':tag': function (node) /*: HTMLElement + Undef -> Str */  {
            return node.tagName;
        },
        ':text': function (node) /*: HTMLElement + Undef -> Bool */  {
            return node.nodeName === '#text';
        },
        ':trim': function (node) /*: HTMLElement + Undef -> Bool */  {
            return node.nodeName !== '#text' || /\W/.test(node.nodeValue);
        },
        ':unchecked': function (node) /*: HTMLElement + Undef -> Bool */  {
            return node.tagName && !node.checked;
        },
        ':visible': function (node) /*: HTMLElement + Undef -> Bool */  {
            return node.tagName && getStyleObject(node).visibility === 'visible';
        }
    };

    function quest(query, nodes)
    /*:  Array<'Selector> + Undef
       * Undef + Array<HTMLElement + Undef>
      -> Undef + Array<HTMLElement + Undef> */
    {
        var selector /*: upcast Undef + 'Selector */,
        func /*: upcast Undef + (HTMLElement + Undef -> Any) */, 
        i = 0,
        j = 0;

        // Step through each selector.

        for (i = 0; i < query.length; i += 1) {
            selector = query[i];
            name = selector.name;
            func = hunter[safe_name(selector.op)];

            // There are two kinds of selectors: hunters and peckers. If this is a hunter,
            // loop through the the nodes, passing each node to the hunter function.
            // Accumulate all the nodes it finds.

            if (typeof func === 'function') {
                if (star) {
                    return error("ADsafe: Query violation: *" +
                                 selector.op + (selector.name || ''));
                }
                result = /*: HTMLElement + Undef */ [];
                for (j = 0; j < nodes.length; j += 1) {
                    func(nodes[j]);
                }
            } else {

                // If this is a pecker, get its function. There is a special case for
                // the :first and :rest selectors because they are so simple.

                value = selector.value;
                flipflop = false;
                func = pecker[safe_name(selector.op)];
                if (typeof func !== 'function') {
                    switch (selector.op) {
                    case ':first':
                        result = nodes.slice(0, 1);
                        break;
                    case ':rest':
                        result = nodes.slice(1);
                        break;
                    default:
                        return error('ADsafe: Query violation: :' + selector.op);
                    }
                } else {

                    // For the other selectors, make an array of nodes that are filtered by
                    // the pecker function.

                    result = /*: HTMLElement + Undef */ [];
                    for (j = 0; j < nodes.length; j += 1) {
                        if (func(nodes[j])) {
                            result.push(nodes[j]);
                        }
                    }
                }
            }
            nodes = result;
        }
        return result;
    }

    function make_root(root, id) 
    /*:   HTMLElement + Undef + Null 
        * Str + Null + Undef 
       -> 'Ad */ 
    {

        if (id) {
            if (root.tagName !== 'DIV') {
                return error('ADsafe: Bad node.');
            }
        } else {
            if (root.tagName !== 'BODY') {
                return error('ADsafe: Bad node.');
            }
        }

        var allow_focus = true,
        dom,
        dom_event = function (e) /*: Event -> Undef */ {
            var key /*: upcast Undef + Str */,

            // The ad can see target, that, and the_event, so we need
            // to annotate and treat them as such.

            target /*: upcast 'Ad */,
            that /*: upcast 'Ad */,
            the_event /*: upcast 'Ad */,
            the_target /*: upcast HTMLElement + Undef */,
            the_actual_event = /*: upcast Event + Undef */ (e || event),
            type = the_actual_event.type;
            
            // Get the target node and wrap it in a bunch.
            
            the_target = the_actual_event.target ||
                the_actual_event.srcElement;

            // The obj* cast tells the type checker that this new object
            // should be treated like the dictionary type 'AdObj.
            // The type checker ensures that the Bunch type doesn't clash
            // with the fields of the 'AdObj type so this cast is valid.
            target = /*: obj* 'AdObj */ (new Bunch([the_target]));
            that = target;
            
            // Use the PPK hack to make focus bubbly on IE.
            // When a widget has focus, it can use the focus method.
            
            switch (type) {
            case 'mousedown':
                allow_focus = true;
                if (document.selection) {
                    the_range = document.selection.createRange();
                }
                break;
            case 'focus':
            case 'focusin':
                allow_focus = true;
                has_focus = the_target;
                the_actual_event.cancelBubble = false;
                type = 'focus';
                break;
            case 'blur':
            case 'focusout':
                allow_focus = false;
                has_focus = null;
                type = 'blur';
                break;
            case 'keypress':
                allow_focus = true;
                has_focus = the_target;
                key = String.fromCharCode(the_actual_event.charCode ||
                                          the_actual_event.keyCode);
                switch (key) {
                case '\u000d':
                case '\u000a':
                    type = 'enterkey';
                    break;
                case '\u001b':
                    type = 'escapekey';
                    break;
                }
                break;

                // This is a workaround for Safari.

            case 'click':
                allow_focus = true;
            }
            if (the_actual_event.cancelBubble &&
                the_actual_event.stopPropagation) {
                the_actual_event.stopPropagation();
            }

            // Make the event object.

            // the_event is ad-visible, so we cast it to an ad-type.  Note 
            // that it has non-banned fields, so all of these have to be
            // cast up to the ad type as well, to satisfy the type checker
            the_event = 
                /*: obj* 'AdObj */
            {
                altKey: /*: upcast 'Ad */ (the_actual_event.altKey),
                ctrlKey: /*: upcast 'Ad */ (the_actual_event.ctrlKey),
                bubble: /*: upcast 'Ad */ 
                (/*: obj* 'AdObj */ 
                    (function () /*: ['Ad + HTMLWindow] 'Ad ... -> 'Ad */ {
                        
                        // Bubble up. Get the parent of that node. It becomes the new that.
                        // the getParent throws when bubbling is not possible.
                        
                        try {
                            var parent = that.getParent(),
                            b = parent.___nodes___[0];
                            that = parent;
                            the_event.that = that;
                            
                            // If that node has an event handler, fire it. Otherwise, bubble up.
                            
                            if (b['___ on ___'] &&
                                b['___ on ___'][type]) {
                                that.fire(the_event);
                            } else {
                                the_event.bubble();
                            }
                        } catch (e) {
                            return error(String(e));
                        }
                    })),
                key: /*: upcast 'Ad */ key,
                preventDefault: /*: upcast 'Ad */ 
                (/*: obj* 'AdObj */ 
                    (function () /*: ['Ad + HTMLWindow] 'Ad ... -> 'Ad */ {
                        if (the_actual_event.preventDefault) {
                            the_actual_event.preventDefault();
                        }
                        the_actual_event.returnValue = false;
                    })),
                shiftKey: /*: upcast 'Ad */ (the_actual_event.shiftKey),
                target: /*: upcast 'Ad */ target,
                that: /*: upcast 'Ad */ that,
                type: /*: upcast 'Ad */ type,
                x: /*: upcast 'Ad */ (the_actual_event.clientX),
                y: /*: upcast 'Ad */ (the_actual_event.clientY)
            };
            that.fire(the_event);
            // If the target has event handlers, then fire them. Otherwise, bubble up.

            if (the_target['___ on ___'] &&
                the_target['___ on ___'][string_check(the_event.type)]) {
                target.fire(the_event);
            } else {
                for (;;) {
                    the_target = the_target.parentNode;
                    if (!the_target) {
                        break;
                    }
                    if (the_target['___ on ___'] &&
                        the_target['___ on ___'][string_check(the_event.type)]) {
                        that = /*: obj* 'AdObj */ (new Bunch([the_target]));
                        the_event.that = that;
                        that.fire(the_event);
                        break;
                    }
                    if (the_target['___adsafe root___']) {
                        break;
                    }
                }
            }
            if (the_event.type === 'escapekey') {
                if (ephemeral) {
                    ephemeral.remove();
                }
                ephemeral = null;
            }
            // This line is annoying --- why add Null to all these types?
            // Change to undefined
            that = the_target = the_event = the_actual_event = undefined;
            return;
        };

        var Bunch_prototype = {
            append : function(appendage) /*: ['Ad + HTMLWindow] 'Ad -> 'Ad */ {
                if (this.window) {
                    return error();
                }
                var b = this.___nodes___,
                flag = false,
                i = 0,
                j = 0,
                node /*: upcast Undef + HTMLElement */,
                rep /*: upcast Undef + Array<HTMLElement + Undef> */;
                if (b.length === 0 || !appendage) {
                    return this;
                }
                if (appendage instanceof Array) {
                    if (appendage.length !== b.length) {
                        return error('ADsafe: Array length: ' +
                                     b.length + '-' + value.length);
                    }
                    for (i = 0; i < b.length; i += 1) {
                        rep = appendage[i].___nodes___;
                        for (j = 0; j < rep.length; j += 1) {
                            b[i].appendChild(rep[j]);
                        }
                    }
                } else {
                    rep = appendage.___nodes___;
                    for (i = 0; i < b.length; i += 1) {
                        node = b[i];
                        for (j = 0; j < rep.length; j += 1) {
                            node.appendChild(flag ?
                                             rep[j].cloneNode(true) : rep[j]);
                        }
                        flag = true;
                    }
                }
                return this;
            },   
            blur : function () /*: ['Ad + HTMLWindow] -> 'Ad */ {
                if (this.window) {
                    return error();
                }
                var b = this.___nodes___, i = 0, node /*: upcast Undef + HTMLElement */;
                has_focus = null;
                for (i = 0; i < b.length; i += 1) {
                    node = b[i];
                    if (node.blur) {
                        node.blur();
                    }
                }
                return this;
            },
            check : function(value) /*: ['Ad + HTMLWindow] 'Ad -> 'Ad */ {
                if (this.window) {
                    return error();
                }
                var b = this.___nodes___, i = 0, node /*: upcast Undef + HTMLElement */;
                if (value instanceof Array) {
                    if (value.length !== b.length) {
                        return error('ADsafe: Array length: ' +
                                     b.length + '-' + value.length);
                    }
                    for (i = 0; i < b.length; i += 1) {
                        node = b[i];
                        if (node.tagName) {
                            node.checked = !!value[i];
                        }
                    }
                } else {
                    for (i = 0; i < b.length; i += 1) {
                        node = b[i];
                        if (node.tagName) {
                            node.checked = !!value;
                        }
                    }
                }
                return this;
            },
            count : function () /*: ['Ad + HTMLWindow] -> Int */ {
                if (this.window) {
                    return error();
                }
                return this.___nodes___.length;
            },
            each : function(func) /*: ['Ad + HTMLWindow] 'Ad -> 'Ad */ {
                if (this.window) {
                    return error();
                }
                var b = this.___nodes___, i = 0;
                if (typeof func === 'function') {
                    for (i = 0; i < b.length; i += 1) {
                        func(/*: obj* 'AdObj */ (new Bunch([b[i]])));
                    }
                    return this;
                }
                return error();
            },




            

        };
        // Mark the node as a root. This prevents event bubbling from propogating
        // past it.

        root['___adsafe root___'] = '___adsafe root___';

        return dom;
    }

    //    function F() {}

    //  Return the ADSAFE object.

    return {

        //        create: typeof Object.create === 'function' ? Object.create : function (o) {
        //            F.prototype = typeof o === 'object' && o ? o : Object.prototype;
        //            return new F();
        //        },

        //  ADSAFE.get retrieves a value from an object.

        get: function(object, name) 
        /*: ['Ad + HTMLWindow] 'Ad * 'Ad * 'Ad ... -> 'Ad */ 
        {
            if (typeof object !== "object") { return error(); }
            if (!reject_name(name)) {
                return object[name];
            }
            return error();
        },


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

        lib: function (name, f) 
        /*: 'Ad * 'Ad -> 'Ad */
        {
            if (reject_name(name)) {
	        return error("ADsafe lib violation.");
            }
            adsafe_lib[name] = f(adsafe_lib);
        },

        //  ADSAFE.log is a debugging aid that spams text to the browser's log.

        log: function(s) 
        /*: Str -> Undef */ 
        {
            if (window.console) {
                console.log(s);        /* Firebug */
            } else if (typeof Debug === 'object') {
                Debug.writeln(s);      /* IE */
            }
        },
        
        //        remove: ADSAFE_remove,

        set: function(object, name, value) 
        /*: ['Ad + HTMLWindow] 'Ad * 'Ad * 'Ad * 'Ad ... -> 'Ad */
        {
            if (typeof object !== "object") { return error(); }
            if (!reject_name(name)) {
                object[name] = value;
                return;
            }
            return error();
        },

        //  ADSAFE._intercept allows the page to register a function that will
        //  see the widget's capabilities.

        //        _intercept: function (f) {
        //            interceptors.push(f);
        //        }

    };
}());