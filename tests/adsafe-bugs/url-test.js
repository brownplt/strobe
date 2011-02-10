function foo(bunch, value) /*: 'Ad * 'Ad -> 'Ad */ {
    if(/url/.test(value)) {
        throw "Contains 'url'";
    }
    return bunch.style.some_entry = value;
}
