compile:
	mkdir -p ebin
	erlc -o ebin src/tld_generator.erl
	# erl -pa ebin -noshell -eval 'tld_generator:generate(file, "publicsuffix.dat")' > src/tld.erl
	erl -pa ebin -noshell -eval 'tld_generator:generate()' > src/tld.erl
	erlc -o ebin src/tld.erl

test: compile
	mkdir -p .eunit
	erlc -o .eunit src/tld.erl tests/tld_tests.erl
	erl +pc unicode -noshell -pa .eunit -eval "eunit:test(tld, [verbose])" -s init stop
