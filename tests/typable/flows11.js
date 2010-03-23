var currentAudioClip = /*:upcast Bool + { stop : -> Void } */ false;

function onPlay() /*: -> Void */ {
    if (typeof currentAudioClip === "boolean") {
    }
    else {
        currentAudioClip.stop();
    }
}