function doNothing() /*: -> Undef */ {}

function string_check(string) /*: Any -> Str */ {

    if(typeof string !== 'string') {
        doNothing();
    }
    return string;
}
