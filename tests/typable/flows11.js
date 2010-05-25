var currentAudioClip = /*:upcast Bool + { stop : -> Undef } */ false;

function onPlay() /*: -> Undef */ {
    if (typeof currentAudioClip === "boolean") {
    }
    else {
        (/*:downcast { stop : -> Undef } */currentAudioClip).stop();
    }
}

