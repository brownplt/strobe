TC=../../tca -disable-unreachable -penv

all: hunterpecker.jsi prelude.jsi query.jsi quest.jsi dom_event.jsi bunch.jsi dom.jsi object.jsi

clean:
	rm -f hunterpecker.jsi prelude.jsi query.jsi dom_event.jsi bunch.jsi dom.jsi object.jsi

prelude.jsi: prelude.js
	$(TC) prelude.js > prelude.jsi

hunterpecker.jsi: hunterpecker.js prelude.jsi
	$(TC) -env prelude.jsi hunterpecker.js > hunterpecker.jsi

query.jsi: hunterpecker.jsi prelude.jsi query.js 
	$(TC) -env prelude.jsi \
              -env hunterpecker.jsi query.js > query.jsi

quest.jsi: hunterpecker.jsi prelude.jsi query.jsi quest.js
	$(TC) -env prelude.jsi \
	      -env hunterpecker.jsi \
	      -env query.jsi quest.js > quest.jsi

dom_event.jsi: hunterpecker.jsi prelude.jsi query.jsi quest.jsi dom_event.js
	$(TC) -env prelude.jsi \
	      -env hunterpecker.jsi \
	      -env query.jsi \
              -env quest.jsi \
              dom_event.js > dom_event.jsi

bunch.jsi: hunterpecker.jsi prelude.jsi query.jsi quest.jsi dom_event.jsi bunch.js
	$(TC) -env prelude.jsi \
	      -env hunterpecker.jsi \
	      -env query.jsi \
              -env quest.jsi \
              -env dom_event.jsi \
	      bunch.js > bunch.jsi

dom.jsi: hunterpecker.jsi prelude.jsi query.jsi quest.jsi dom_event.jsi bunch.jsi dom.js
	$(TC) -env prelude.jsi \
	      -env hunterpecker.jsi \
	      -env query.jsi \
              -env quest.jsi \
              -env dom_event.jsi \
	      -env bunch.jsi \
		dom.js > dom.jsi

object.jsi: prelude.jsi dom.jsi dom_event.jsi bunch.jsi object.js
	$(TC) -env prelude.jsi \
	      -env dom.jsi \
	      -env dom_event.jsi \
	      -env bunch.jsi \
	      object.js > object.jsi