TC=../../tcb -penv

all: hunterpecker.jsi prelude.jsi

prelude.jsi: prelude.js
	$(TC) prelude.js > prelude.jsi

hunterpecker.jsi: hunterpecker.js prelude.jsi
	$(TC) -env prelude.jsi hunterpecker.js > hunterpecker.jsi