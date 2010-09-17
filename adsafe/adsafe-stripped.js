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


//  The reject function enforces the restriction on get and put.
//  It allows access only to objects and arrays. It does not allow use of
//  the banned names, or names that are not strings or positive numbers,
//  or strings that start or end with _ or strings that start with -.


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
        };

// Return an ADsafe dom object.

        dom = {
            count: function () {
                return 1;
            },
            ephemeral: function (bunch) {
                if (ephemeral) {
                    ephemeral.remove();
                }
                ephemeral = bunch;
                return dom;
            },
            fragment: function () {
                return new Bunch([document.createDocumentFragment()]);
            },
            remove: function () {
                purge_event_handlers(root);
                root.parent.removeElement(root);
                root = null;
            },
            tag: function (tag, type, name) {
                var node;
                if (typeof tag !== 'string') {
                    return error();
                }
                if (makeableTagName[tag] !== true) {
                    return error('ADsafe: Bad tag: ' + tag);
                }
                node = document.createElement(tag);
                if (name) {
                    node.autocomplete = 'off';
                    node.name = name;
                }
                if (type) {
                    node.type = type;
                }
                return new Bunch([node]);
            },
            text: function (text) {
                var a, i;
                if (text instanceof Array) {
                    a = [];
                    for (i = 0; i < text.length; i += 1) {
                        a[i] = document.createTextNode(String(text[i]));
                    }
                    return new Bunch(a);
                }
                return new Bunch([document.createTextNode(String(text))]);
            },
            append: function (bunch) {
                var b = bunch.___nodes___, i, n;
                for (i = 0; i < b.length; i += 1) {
                    n = b[i];
                    if (typeof n === 'string' || typeof n === 'number') {
                        n = document.createTextNode(String(n));
                    }
                    root.appendChild(n);
                }
                return dom;
            },
            prepend: function (bunch) {
                var b = bunch.___nodes___, i;
                for (i = 0; i < b.length; i += 1) {
                    root.insertBefore(b[i], root.firstChild);
                }
                return dom;
            },
            row: function (values) {
                var tr = document.createElement('tr'),
                    td,
                    i;
                for (i = 0; i < values.length; i += 1) {
                    td = document.createElement('td');
                    td.appendChild(document.createTextNode(String(values[i])));
                    tr.appendChild(td);
                }
                return new Bunch([tr]);
            }
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

        lib: function (name, f) {
            if (!adsafe_id) {
                return error();
            }
            adsafe_lib[name] = f(adsafe_lib);
        },


//  ADSAFE.log is a debugging aid that spams text to the browser's log.

        log: log,


//  ADSAFE.remove deletes a value from an object.

        remove: function (object, name) {
            if (arguments.length === 2 && !reject(object, name)) {
                delete object[name];
                return;
            }
            return error();
        },


//  ADSAFE.set stores a value in an object.

        set: function (object, name, value) {
            if (arguments.length === 3 && !reject(object, name)) {
                object[name] = value;
                return;
            }
            return error();
        },

//  ADSAFE._intercept allows the page to register a function that will
//  see the widget's capabilities.

        _intercept: function (f) {
            interceptors.push(f);
        }

    };
}());