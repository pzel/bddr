.PHONY: test-clean test-compile test compile

BEAMPATH=_build/default/lib/bddr/ebin
REBAR=./priv/rebar3

test: test-clean test-compile compile
	@erl -shutdown_time 1 -noshell -pa $(BEAMPATH) -pa test \
	-eval 'bddr_suite:run_suite(bddr_test_SUITE).'

compile:
	@$(REBAR) compile

test-clean:
	@rm -f ./ebin/*beam; rm -f ./test/*beam

test-compile:
	@erlc -I ./.. -pa $(BEAMPATH) -pa test -o test -Werror test/*.erl


