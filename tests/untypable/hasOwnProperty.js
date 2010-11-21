function foo(obj, name) /*: {x: Int, #proto: Object, *: Bool, #code: Bot} * Str -> Int + Bool */ {

    if(obj.hasOwnProperty(name)) {
        return obj[name];
    }
    else {
        return false;
    }

}