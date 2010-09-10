// Why are these globals? Just to make our lives harder?
var name = ""; 
var value = /*: upcast Any*/undefined;
var result = /*: HTMLElement */ [];
var star = false;
var flipflop = true;
var has_focus = true;

var hunter = 
    /*: upcast {"": HTMLElement + Undef -> Undef,
                "+": HTMLElement + Undef -> Undef, 
                ">": HTMLElement + Undef -> Undef,
                "#": HTMLElement + Undef -> Undef,
                "/": HTMLElement + Undef -> Undef,
                "*": HTMLElement + Undef -> Undef,
                #proto: Object, *: Undef, #code: Undef} */ 
{

    // These functions implement the hunter behaviors.

    '': function (node) /*: HTMLElement + Undef -> Undef */ {
        var e = node.getElementsByTagName(name);
        for (var i = 0; i < 1000; i += 1) {
            // Why bound at 1000?
            if (/*: cheat Bool*/(e[i])) {
                result.push(/*:cheat HTMLElement */ (e[i]));
            } else {
                break;
            }
        }
    },

    '+': function (node) /*: HTMLElement + Undef -> Undef */ {
        node = node.nextSibling;
        name = name.toUpperCase();
        while (/*: cheat Bool*/(node && !node.tagName)) {
            node = node.nextSibling;
        }
        if (/*:cheat Bool */(node && node.tagName === name)) {
            result.push(/*: cheat HTMLElement */node);
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
        if (/*: cheat Bool*/(n.tagName)) {
            result.push(/*: cheat HTMLElement*/n);
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
        walkTheDOM(/*: cheat HTMLElement */ node, function (node) /*: Node -> Undef */ {
            result.push(/*:cheat HTMLElement */node);
        }, true);
    }
};

var pecker = 
    /*: upcast {".":   HTMLElement + Undef -> Bool ,
                "&":   HTMLElement + Undef -> Bool ,
                "_":   HTMLElement + Undef -> Bool ,
                "[":   HTMLElement + Undef -> Bool ,
                "[=":  HTMLElement + Undef -> Bool ,
                "[!=": HTMLElement + Undef -> Bool ,
                "[^=": HTMLElement + Undef -> Bool ,
                "[$=": HTMLElement + Undef -> Bool ,
                "[*=": HTMLElement + Undef -> Bool ,
                "[~=": HTMLElement + Undef -> Bool ,
                "[|=": HTMLElement + Undef -> Bool ,
                ":blur": HTMLElement + Undef -> Bool ,
                ":checked": HTMLElement + Undef -> Bool ,
                ":disabled": HTMLElement + Undef -> Bool ,
                ":enabled": HTMLElement + Undef -> Bool ,
                ":even": HTMLElement + Undef -> Bool ,
                ":focus": HTMLElement + Undef -> Bool ,
                ":hidden": HTMLElement + Undef -> Bool ,
                ":odd": HTMLElement + Undef -> Bool ,
                ":tag": HTMLElement + Undef -> Str ,
                ":text": HTMLElement + Undef -> Bool ,
                ":trim": HTMLElement + Undef -> Bool ,
                ":unchecked": HTMLElement + Undef -> Bool ,
                ":visible": HTMLElement + Undef -> Bool ,
                #proto: Object, *: Undef, #code: Undef}
    */
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
        return typeof /*: cheat Str */ (node[name]) === 'string';
    },
    '[=': function (node) /*: HTMLElement + Undef -> Bool */  {
        var member = /*: cheat Any */ (node[name]);
        return typeof member === 'string' && member === value;
    },
    '[!=': function (node) /*: HTMLElement + Undef -> Bool */  {
        var member = /*: cheat Any */ (node[name]);
        return typeof member === 'string' && member !== value;
    },
    '[^=': function (node) /*: HTMLElement + Undef -> Bool */  {
        var member = /*: cheat Any */ (node[name]);
        return typeof member === 'string' &&
            member.slice(0, member.length) === value;
    },
    '[$=': function (node) /*: HTMLElement + Undef -> Bool */  {
        var member = /*: cheat Any */ (node[name]);
        return typeof member === 'string' &&
            member.slice(-member.length) === value;
    },
    '[*=': function (node) /*: HTMLElement + Undef -> Bool */  {
        var member = /*: cheat Any */ (node[name]);
        return typeof member === 'string' &&
            /*: cheat Bool */ (member.slice.indexOf(value) >= 0);
    },
    '[~=': function (node) /*: HTMLElement + Undef -> Bool */  {
        var member = /*: cheat Any */ (node[name]);
        return typeof member === 'string' &&
            /*: cheat Bool */ ((' ' + member + ' ').slice.indexOf(' ' + value + ' ') >= 0);
    },
    '[|=': function (node) /*: HTMLElement + Undef -> Bool */  {
        var member = /*: cheat Any */ (node[name]);
        return typeof member === 'string' &&
            /*: cheat Bool */ (('-' + member + '-').slice.indexOf('-' + value + '-') >= 0);
    },
    ':blur': function (node) /*: HTMLElement + Undef -> Bool */  {
        return node !== has_focus;
    },
    ':checked': function (node) /*: HTMLElement + Undef -> Bool */  {
        return node.checked;
    },
    ':disabled': function (node) /*: HTMLElement + Undef -> Bool */  {
        return /*: cheat Bool */(node.tagName && node.disabled);
    },
    ':enabled': function (node) /*: HTMLElement + Undef -> Bool */  {
        return /*: cheat Bool */ (node.tagName && !node.disabled);
    },
    ':even': function (node) /*: HTMLElement + Undef -> Bool */  {
        var f /*: upcast Bool + Undef */;
        if (/*: cheat Bool */ (node.tagName)) {
            f = flipflop;
            flipflop = !flipflop;
            return /*: cheat Bool */ f;
        } else {
            return false;
        }
    },
    ':focus': function (node) /*: HTMLElement + Undef -> Bool */  {
        return node === has_focus;
    },
    ':hidden': function (node) /*: HTMLElement + Undef -> Bool */  {
        return /*: cheat Bool */ (node.tagName) && 
            getStyleObject(/*: cheat HTMLElement */ node).visibility !== 'visible';
    },
    ':odd': function (node) /*: HTMLElement + Undef -> Bool */  {
        if (/*: cheat Bool */ node.tagName) {
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
        return /*: cheat Bool */ node.tagName && !node.checked;
    },
    ':visible': function (node) /*: HTMLElement + Undef -> Bool */  {
        return /*: cheat Bool */ node.tagName && getStyleObject(/*: cheat HTMLElement */ node).visibility === 'visible';
    }
};
