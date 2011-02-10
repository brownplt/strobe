function getStyle(name) /*: ['Ad] 'Ad -> 'Ad */ {
    var sty = getComputedStyle(this.___nodes___[0], '');
    return sty[name];
}