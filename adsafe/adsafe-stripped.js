var ADSAFE = (function () {

    var adsafe_id,      // The id of the current widget
        adsafe_lib,     // The script libraries loaded by the current widget

// These member names are banned from guest scripts. The ADSAFE.get and
// ADSAFE.put methods will not allow access to these properties.


        cache_style_object,
        cache_style_node,
        defaultView = document.defaultView,
        ephemeral,
        flipflop,       // Used in :even/:odd processing
        has_focus,
        hunter,         // Set of hunter patterns
        interceptors = [],

        result,

        the_range,
        value;

//  Firefox implemented some of its array methods carelessly. If a method is
//  called as a function it returns the global object. ADsafe cannot tolerate
//  that, so we wrap the methods to make them safer and slower.

    function mozilla(name) {
        var method = Array.prototype[name];
        Array.prototype[name] = function () {
            if (this === this.window) {
                return error();
            }
            return method.apply(this, arguments);
        };
    }

    mozilla('concat');
    mozilla('every');
    mozilla('filter');
    mozilla('forEach');
    mozilla('map');
    mozilla('reduce');
    mozilla('reduceRight');
    mozilla('reverse');
    mozilla('slice');
    mozilla('some');
    mozilla('sort');

    function make_root(root, id) {

        if (id) {
            if (root.tagName !== 'DIV') {
                return error('ADsafe: Bad node.');
            }
        } else {
            if (root.tagName !== 'BODY') {
                return error('ADsafe: Bad node.');
            }
        }

// A Bunch is a container that holds zero or more dom nodes.
// It has many useful methods.

        function Bunch(nodes) {
            this.___nodes___ = nodes;
            this.___star___ = star && nodes.length > 1;
            star = false;
        }

        var allow_focus = true,
        dom;

// Mark the node as a root. This prevents event bubbling from propogating
// past it.

        root['___adsafe root___'] = '___adsafe root___';

        Bunch.prototype = {
            append: Bunch_append,
            blur: Bunch_blur,
            check: Bunch_check,
            count: Bunch_count,
            empty: Bunch_empty,
            enable: Bunch_enable,
            ephemeral: Bunch_ephemeral,
            explode: Bunch_explode,
            fire: Bunch_fire,
            focus: Bunch_focus,
            each: Bunch_each,
            fragment: Bunch_fragment,
            getCheck: Bunch_getCheck,
            getClass: Bunch_getClass,
            getMark: Bunch_getMark,
            getName: Bunch_getName,
            getOffsetHeight: Bunch_getOffsetHeight,
            getParent: Bunch_getParent,
            getSelection: Bunch_getSelection,
            getStyle: Bunch_getStyle,
            getTagName: Bunch_getTagName,
            getTitle: Bunch_getTitle,
            getValue: Bunch_getValue,
            klass: Bunch_klass,
            mark: Bunch_mark,
            off: Bunch_off,
            on: Bunch_on,
            protect: Bunch_protect,
            q: Bunch_q,
            remove: Bunch_remove,
            replace: Bunch_replace,
            select: Bunch_select,
            selection: Bunch_selection,
            style: Bunch_style,
            tag: Bunch_tag,
            text: Bunch_text,
            title: Bunch_title,
            value: Bunch_value
        };

// Return an ADsafe dom object.

        dom = {
        };

        if (typeof root.addEventListener === 'function') {
            root.addEventListener('focus', dom_event, true);
            root.addEventListener('blur', dom_event, true);
            root.addEventListener('mouseover', dom_event, true);
            root.addEventListener('mouseout', dom_event, true);
            root.addEventListener('mouseup', dom_event, true);
            root.addEventListener('mousedown', dom_event, true);
            root.addEventListener('mousemove', dom_event, true);
            root.addEventListener('click', dom_event, true);
            root.addEventListener('dblclick', dom_event, true);
            root.addEventListener('keypress', dom_event, true);
        } else {
            root.onfocusin = root.onfocusout  = root.onmouseover =
                             root.onmouseout  = root.onmouseup   =
                             root.onmousedown = root.onmousemove =
                             root.onclick     = root.ondblclick  =
                             root.onkeypress  = dom_event;
        }
        return [dom, Bunch.prototype];
    }


    function F() {}


//  Return the ADSAFE object.

    return {

        create: typeof Object.create === 'function' ? Object.create : function (o) {
            F.prototype = typeof o === 'object' && o ? o : Object.prototype;
            return new F();
        },

//  ADSAFE.get retrieves a value from an object.

        get: ADSAFE_get,

//  ADSAFE.go allows a guest widget to get access to a wrapped dom node and
//  approved ADsafe libraries. It is passed an id and a function. The function
//  will be passed the wrapped dom node and an object containing the libraries.


        go: function (id, f) {
            var dom, fun, root, i, scripts;

//  If ADSAFE.id was called, the id better match.

            if (adsafe_id && adsafe_id !== id) {
                return error();
            }

//  Get the dom node for the widget's div container.

            root = document.getElementById(id);
            if (root.tagName !== 'DIV') {
                return error();
            }
            adsafe_id = null;

//  Delete the scripts held in the div. They have all run, so we don't need
//  them any more. If the div had no scripts, then something is wrong.
//  This provides some protection against mishaps due to weakness in the
//  document.getElementById function.

            scripts = root.getElementsByTagName('script');
            i = scripts.length - 1;
            if (i < 0) {
                return error();
            }
            do {
                root.removeChild(scripts[i]);
                i -= 1;
            } while (i >= 0);
            root = make_root(root, id);
            dom = root[0];


// If the page has registered interceptors, call then.

            for (i = 0; i < interceptors.length; i += 1) {
                fun = interceptors[i];
                if (typeof fun === 'function') {
                    try {
                        fun(id, dom, adsafe_lib, root[1]);
                    } catch (e1) {
                        ADSAFE.log(e1);
                    }
                }
            }

//  Call the supplied function.

            try {
                f(dom, adsafe_lib);
            } catch (e2) {
                ADSAFE.log(e2);
            }
            root = null;
            adsafe_lib = null;
        },

//  ADSAFE.id allows a guest widget to indicate that it wants to load
//  ADsafe approved libraries.

        id: function (id) {

//  Calls to ADSAFE.id must be balanced with calls to ADSAFE.go.
//  Only one id can be active at a time.

            if (adsafe_id) {
                return error();
            }
            adsafe_id = id;
            adsafe_lib = {};
        },

//  ADSAFE.isArray returns true if the operand is an array.

        isArray: Array.isArray || function (value) {
            return Object.prototype.toString.apply(value) === '[object Array]';
        },


//  ADSAFE.later calls a function at a later time.

        later: function (func, timeout) {
            if (typeof func === 'function') {
                setTimeout(func, timeout || 0);
            } else {
                return error();
            }
        },


//  ADSAFE.lib allows an approved ADsafe library to make itself available
//  to a widget. The library provides a name and a function. The result of
//  calling that function will be made available to the widget via the name.

        lib: lib,

//  ADSAFE.log is a debugging aid that spams text to the browser's log.

        log: log,

        remove: ADSAFE_remove,

        set: ADSAFE_set,

//  ADSAFE._intercept allows the page to register a function that will
//  see the widget's capabilities.

        _intercept: function (f) {
            interceptors.push(f);
        }

    };
}());