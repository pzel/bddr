.PHONY: test-clean test-compile test compile

BEAMPATH=_build/default/lib/bddr/ebin

test: test-clean test-compile compile
	@erl -shutdown_time 1 -noshell -pa $(BEAMPATH) -pa test \
	-eval 'bddr_suite:run_suite(bddr_test_SUITE).'

compile:
	@rebar3 compile

test-clean:
	@rm -f ./ebin/*beam; rm -f ./test/*beam

test-compile:
	@erlc -I ./.. -pa ebin -pa test -o test -Werror test/*.erl


