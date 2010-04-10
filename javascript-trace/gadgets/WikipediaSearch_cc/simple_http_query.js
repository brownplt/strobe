// A general function to simplify downloading data from a website and also
// handle errors gracefully.
function SimpleHTTPRequest() {
  var getStream_ = false;
  var request_ = null;
  var stop_ = false;
  var receivedResultCallback_ = null;

  // Do a request to get a webpage. If getText is true, a text version of the
  // output will be returned. If false, the response stream will be returned
  // (useful for getting image data).
  function request(url, receivedResultCallback, opt_getStream) {
    assert(receivedResultCallback != null);
    if (receivedResultCallback == null)
      return;
    receivedResultCallback_ = receivedResultCallback;
    assert(request_ == null);
    if (request_ != null) {
      receivedResultCallback_(null);
      return;
    }

    request_ = new XMLHttpRequest();

    // Catch any URL errors
    try {
      request_.open("GET", url, true);
    } catch (e) {
      request_ = null;
      receivedResultCallback_(null);
      return;
    }

    request_.onreadystatechange = onData;
    stop_ = false;

    var getStream = false;
    if ((opt_getStream !== undefined) && (opt_getStream === true))
      getStream = true;
    getStream_ = getStream;

    // Send the request
    try {
      request_.send();
    } catch(e) {
      request_ = null;
      receivedResultCallback_(null);
      return;
    }
  }

  // Stop the current transfer. No data callbacks will be made for this request.
  function stop() {
    if (request_ == null)
      return;

    stop_ = true;
    request_.abort();
    request_ = null;
  }

  // Called when data is received from the website
  function onData() {
    // If the download has been stopped, do not continue
    if (stop_)
      return;

    // If the data is not ready (still downloading), do not continue
    assert(request_ != null);
    if (request_.readyState != 4)
      return;

    // If there was an http error, do not continue
    if (request_.status != 200) {
      receivedResultCallback_(null);
      request_ = null;
      return;
    }

    // Return the requested data
    if (getStream_) {
      receivedResultCallback_(request_.responseStream);
    } else {
      receivedResultCallback_(request_.responseText);
    }

    request_ = null;
  }

  this.request = request;
  this.stop = stop;
}
