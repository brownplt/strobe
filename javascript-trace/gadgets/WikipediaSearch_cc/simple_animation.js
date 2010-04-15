// This class simply wraps around the framework's animation methods, but adds
// a callback for when the animation is completed
function SimpleAnimation(animateCallback_, startValue_, endValue_, duration_,
    completedCallback_) {
  var animation_ = null;
  var timer_ = null;

  function start() {
    animation_ = beginAnimation(onAnimate, startValue_, endValue_, duration_);
    timer_ = setTimeout(onAnimationComplete, duration_);
  }

  function stop() {
    if (animation_ != null)
      view.cancelAnimation(animation_);
    if (timer_ != null)
      view.clearInterval(timer_);
  }

  function onAnimate() {
    // Do not animate if stopped
    if (timer_ == null)
      return;

    animateCallback_(event.value);
  }

  function onAnimationComplete() {
    // Kill the timer/animation
    stop();

    completedCallback_();
  }

  this.start = start;
  this.stop = stop;
}