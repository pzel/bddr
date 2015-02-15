.PHONY: test-clean test-compile test compile

test: test-clean test-compile compile
	@erl -shutdown_time 10 -noshell -pa ebin -pa test \
	-eval 'bddr_suite:run_suite(bddr_SUITE).'

compile:
	@rebar compile

test-clean:
	@rm -f ./ebin/*beam; rm -f ./test/*beam

test-compile:
	@erlc -I ./.. -pa ebin -pa test -o test -Werror test/*.erl


