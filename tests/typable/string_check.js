var error = /*: cheat Any -> Bot */ null;
function string_check(string) /*: Any -> Str */ {
    if (typeof string !== 'string') {
            return error("ADsafe string violation." + String(string));
    }
    return string;
}
