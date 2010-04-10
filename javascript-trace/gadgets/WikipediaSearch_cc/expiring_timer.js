// ExpiringTimer is a timer that acts like a regular timer and triggers an
// event at a certain interval. The difference, however, is that an
// ExpiringTimer will stop triggering events after a certain amount of time if
// not pinged. When both interval and expireAfter are the same, this can be
// used as a method of preventing "too-fast" situations. This forces the
// callback to happen at most once every interval.
function ExpiringTimer(interval_, expireAfter_, callback_) {
  var expireTimer_ = null;
  var timer_ = null;

  // Resets all the expiry timer and ensure the main timer is running.
  // opt_callbackNow, if true, will callback immediately if the expiry timer is
  // not already running.
  function ping(opt_callbackNow) {
    var callbackNow = false;

    // Restart the main timer if it is not already running
    if (timer_ == null)
      timer_ = setInterval(onTimer, interval_);

    // Stop the expire timer if it exists because we will be resetting it
    if (expireTimer_ != null) {
      clearInterval(expireTimer_);
      expireTimer_ = null;
    } else {
      if ((opt_callbackNow) && (opt_callbackNow === true))
        callbackNow = true;
    }
    expireTimer_ = setTimeout(onExpireTimer, expireAfter_);

    if (callbackNow)
      callback_();
  }

  function onTimer() {
    callback_();
  }

  function onExpireTimer() {
    expireTimer_ = null;

    // Kill the main timer since we have expired
    if (timer_ != null) {
      clearInterval(timer_);
      timer_ = null;
    }
  }

  this.ping = ping;
}
