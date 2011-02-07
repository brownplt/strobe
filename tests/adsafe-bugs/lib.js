function error() /*: -> Bot */ { throw "error"; }
var adsafe_lib = /*: obj* 'AdObj */ {};
var adsafe_id /*: upcast Str + Undef */;
function lib(name, f) /*: 'Ad * 'Ad -> 'Ad */ {
    if (!adsafe_id) {
        return error();
    }
    adsafe_lib[name] = f(adsafe_lib);
}
