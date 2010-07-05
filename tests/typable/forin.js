function(obj) /*: Any -> Str */ {
    for (var ix in obj) {
        var r = obj[ix];
        if (typeof r === "string") {
            return r;
        }
    }
    return "no string found";
};