var makeableTagName = {"div": true, "p": true};

function tag(name) /*: 'Ad -> 'Ad */ {
    var node /*: upcast HTMLElement + Undef */;
    if(makeableTagName[name] === true) {
        node = document.createElement(name);
    }
}