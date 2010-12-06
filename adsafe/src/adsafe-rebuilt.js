var ADSAFE = (function () {

    var adsafe_id,      // The id of the current widget
        adsafe_lib,     // The script libraries loaded by the current widget

// These member names are banned from guest scripts. The ADSAFE.get and
// ADSAFE.put methods will not allow access to these properties.


        cache_style_node /*: upcast Undef + HTMLElement */,
        cache_style_object /*: upcast Undef + 'Style */,
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

        go: ADSAFE_go,

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

        later: later,

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