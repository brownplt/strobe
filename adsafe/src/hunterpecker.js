// Why are these globals? Just to make our lives harder?
var name = /*: cheat Str*/undefined; 
var value = /*: upcast Any*/undefined;
var result = /*: HTMLElement*/ [];

var hunter = {

    // These functions implement the hunter behaviors.

    '': function (node) /*: HTMLElement -> Undef */ {
        var e = node.getElementsByTagName(name);
        for (var i = 0; i < 1000; i += 1) {
            // Why bound at 1000?
            if (/*: cheat Bool*/(e[i])) {
                result.push(e[i]);
            } else {
                break;
            }
        }
    },

    '+': function (node) /*: HTMLElement + Undef -> Undef */ {
        node = node.nextSibling;
        name = name.toUpperCase();
        while (/*: cheat Bool*/(node && !node.tagName)) {
            node = (/*:cheat HTMLElement */node).nextSibling;
        }
        if (/*:cheat Bool */(node && node.tagName === name)) {
            result.push(/*: cheat HTMLElement*/node);
        }
    },

    '>': function (node) /*: HTMLElement + Undef -> Undef */ {
        node = node.firstChild;
        name = name.toUpperCase();
        while (/*:cheat Bool*/node) {
            if (node.tagName === name) {
                result.push(/*: cheat HTMLElement*/node);
            }
            node = node.nextSibling;
        }
    },

    '#': function (node) /*: Any -> Undef */ {
        var n = document.getElementById(name);
        if (/*: cheat Bool*/(n.tagName)) {
            result.push(/*: cheat HTMLElement*/n);
        }
    },
    '/': function (node) /*: HTMLElement -> Undef */{
        var e = node.childNodes;
        for (var i = 0; i < e.length; i += 1) {
            result.push(e[i]);
        }
    }, /* TODO: walkTheDOM
    '*': function (node) {
        star = true;
        walkTheDOM(node, function (node) {
            result.push(node);
        }, true);
    }*/
};

var pecker = {
    '.': function (node) /*: HTMLElement -> Bool */ {
        return (' ' + node.className + ' ').indexOf(' ' + name + ' ') >= 0;
    },

    '&': function (node) /*: HTMLElement -> Bool */ {
        return node.name === name;
    }, /*
    '_': function (node) {
        return node.type === name;
    },
    '[': function (node) {
        return typeof node[name] === 'string';
    },
    '[=': function (node) {
        var member = node[name];
        return typeof member === 'string' && member === value;
    },
    '[!=': function (node) {
        var member = node[name];
        return typeof member === 'string' && member !== value;
    },
    '[^=': function (node) {
        var member = node[name];
        return typeof member === 'string' &&
            member.slice(0, member.length) === value;
    },
    '[$=': function (node) {
        var member = node[name];
        return typeof member === 'string' &&
            member.slice(-member.length) === value;
    },
    '[*=': function (node) {
        var member = node[name];
        return typeof member === 'string' &&
            member.slice.indexOf(value) >= 0;
    },
    '[~=': function (node) {
        var member = node[name];
        return typeof member === 'string' &&
            (' ' + member + ' ').slice.indexOf(' ' + value + ' ') >= 0;
    },
    '[|=': function (node) {
        var member = node[name];
        return typeof member === 'string' &&
            ('-' + member + '-').slice.indexOf('-' + value + '-') >= 0;
    },
    ':blur': function (node) {
        return node !== has_focus;
    },
    ':checked': function (node) {
        return node.checked;
    },
    ':disabled': function (node) {
        return node.tagName && node.disabled;
    },
    ':enabled': function (node) {
        return node.tagName && !node.disabled;
    },
    ':even': function (node) {
        var f;
        if (node.tagName) {
            f = flipflop;
            flipflop = !flipflop;
            return f;
        } else {
            return false;
        }
    },
    ':focus': function (node) {
        return node === has_focus;
    },
    ':hidden': function (node) {
        return node.tagName && getStyleObject(node).visibility !== 'visible';
    },
    ':odd': function (node) {
        if (node.tagName) {
            flipflop = !flipflop;
            return flipflop;
        } else {
            return false;
        }
    },
    ':tag': function (node) {
        return node.tagName;
    },
    ':text': function (node) {
        return node.nodeName === '#text';
    },
    ':trim': function (node) {
        return node.nodeName !== '#text' || /\W/.test(node.nodeValue);
    },
    ':unchecked': function (node) {
        return node.tagName && !node.checked;
    },
    ':visible': function (node) {
        return node.tagName && getStyleObject(node).visibility === 'visible';
    } */
};
