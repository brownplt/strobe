var assert = function(cond) /*: Bool -> Undef */ {
  if (!cond) {
    throw 0;
  }
};

function SimpleHTTPRequest() /*: -> Undef */ {
  var receivedResultCallback_ = /*:upcast (Null + (String -> Undef)) */null;

  // Do a request to get a webpage. If getText is true, a text version of the
  // output will be returned. If false, the response stream will be returned
  // (useful for getting image data).
  function request(url, receivedResultCallback, opt_getStream) /*: String * (String -> Undef) * (Boolean + Undef) -> Undef */ {
    assert(receivedResultCallback != null);
    receivedResultCallback_ = receivedResultCallback;
    receivedResultCallback_("STRING");
  }
}

