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

use-default:
	rm skhdrc || true
	ln -s skhdrc.default skhdrc

use-hhkb-lite-2:
	rm skhdrc || true
	ln -s skhdrc.hhkb-lite-2 skhdrc
