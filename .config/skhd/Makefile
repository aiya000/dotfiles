check-health:
	if ! which skhd > /dev/null ; then \
		echo "skhd is not healthy (not installed)" && \
		exit 1 ; \
	fi
	skhd --reload \
		&& echo "skhd is healthy (reloaded)" \
		|| echo "skhd is not healthy (may not be running)"

start:
	skhd --start-service

stop:
	skhd --stop-service

restart:
	$(MAKE) stop
	$(MAKE) start

use-default:
	rm skhdrc || true
	cat skhdrc.common skhdrc.default > skhdrc
	$(MAKE) restart

use-hhkb-lite-2:
	rm skhdrc || true
	cat skhdrc.common skhdrc.hhkb-lite-2 > skhdrc
	$(MAKE) restart
