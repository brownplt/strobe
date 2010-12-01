//  The reject function enforces the restriction on get and put.
//  It allows access only to objects and arrays. It does not allow use of
//  the banned names, or names that are not strings or positive numbers,
//  or strings that start or end with _ or strings that start with -.

function reject(object, name) 
/*: 'Ad * 'Ad -> Any */ 
{
    return typeof object !== 'object'  || /*: cheat Any */ (banned[name]) ||
        ((typeof name !== 'number' || name < 0) &&
         (typeof name !== 'string'  || /*: cheat Str */ name.charAt(0) === '_' ||
          /*: cheat Str */ name.slice(-1) === '_'     || /*: cheat Str */ name.charAt(0) === '-'));
}

function ADSAFE_get(object, name) 
/*: 'Ad * 'Ad -> 'Ad */ 
{
    if (!reject(object, name)) {
        return /*: cheat 'Ad */ (object[/*: cheat Str */name]);
    }
    return error();
}

//  ADSAFE.remove deletes a value from an object.

function ADSAFE_remove (object, name) 
/*: 'Ad * 'Ad -> 'Ad */
{
    if (!reject(object, name)) {
        /*: cheat Bool */ (delete object[/*: cheat Str + Int */ name]);
        return;
    }
    return error();
}

//  ADSAFE.set stores a value in an object.

function ADSAFE_set (object, name, value) 
/*: 'Ad * 'Ad * 'Ad -> 'Ad */
{
    if (/* arguments.length === 3 && */ !reject(object, name)) {
        /*: cheat 'Ad */ (object[/*: cheat Str */ name] = value);
        return;
    }
    return error();
}

function make_root(root, id) /*: HTMLElement + Undef + Null * Str + Null + Undef -> 'Ad */ {

    if (id) {
        if (root.tagName !== 'DIV') {
            return error('ADsafe: Bad node.');
        }
    } else {
        if (root.tagName !== 'BODY') {
            return error('ADsafe: Bad node.');
        }
    }

    // Mark the node as a root. This prevents event bubbling from propogating
    // past it.

    root['___adsafe root___'] = '___adsafe root___';

    /*: cheat 'Ad */ (Bunch.prototype = { // Need to make this assignment work
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
    });

    // Return an ADsafe dom object.

    dom = /*: cheat 'Ad */ dom_outer;

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
    return dom;
}



function ADSAFE_go (id, f) /*: Str * ('Ad * 'Ad -> 'Ad) -> Undef */ {
    var dom /*: upcast 'Ad */, fun, root /*: upcast HTMLElement + Undef + Null */, i = 0, scripts /*: upcast Undef + Array<HTMLElement>*/;

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
    //    root = make_root(root, id);   // Change this to just return dom (ignoring interceptors)
    dom = make_root(root, id);
    
    
    // If the page has registered interceptors, call then.
    
//    for (i = 0; i < interceptors.length; i += 1) {
//        fun = interceptors[i];
//        if (typeof fun === 'function') {
//            try {
//                fun(id, dom, adsafe_lib, root[1]);
//            } catch (e1) {
//                ADSAFE.log(e1);
//            }
//        }
//    }
    
    //  Call the supplied function.
    
    try {
        f(dom, adsafe_lib);
    } catch (e2) {
//        ADSAFE.log(e2);
    }
    root = null;
    adsafe_lib = null;
}
