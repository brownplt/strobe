TC=../../tcb -penv -env adsafe.env

all: hunterpecker.jsi prelude.jsi

clean:
	rm -f hunterpecker.jsi prelude.jsi query.jsi

prelude.jsi: prelude.js
	$(TC) prelude.js > prelude.jsi

hunterpecker.jsi: hunterpecker.js prelude.jsi
	$(TC) -env prelude.jsi hunterpecker.js > hunterpecker.jsi

query.jsi: hunterpecker.jsi
	$(TC) -env hunterpecker.jsi query.js > query.jsi
