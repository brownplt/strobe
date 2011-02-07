var ephemeral /*: upcast 'Ad */;
function ephemeral_method() /*: ['Ad + HTMLWindow] -> 'Ad */ {
    if (ephemeral) {
        ephemeral.remove();
    }
    ephemeral = this;
    return this;
}
