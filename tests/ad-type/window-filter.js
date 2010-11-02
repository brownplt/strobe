function foo() /*: ['Ad + HTMLWindow] -> 'Ad */ {
    if(this.window) {
        return null;
    }

    return this;

}
