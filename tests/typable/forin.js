function(obj) /*: { * : Any, __proto__: Null } -> Str */ {
    for (var ix in obj) {
        var r = obj[ix];
        if (typeof r === "string") {
            return r;
        }
    }
    return "no string found";
};
