TC=../../tcb -penv -env adsafe.env

all: hunterpecker.jsi prelude.jsi query.jsi quest.jsi dom_event.jsi

clean:
	rm -f hunterpecker.jsi prelude.jsi query.jsi

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
