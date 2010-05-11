all: tests

tests: compile
	erl -pa ./ebin ../erlsom/ebin -noshell -eval 'all_tests:run(), halt().'

compile: prepare 
	erlc -o ebin src/*.erl
	erlc -o ebin tests/*.erl
	
examples: prepare_examples
	erlc -pa ebin -o ebin/examples examples/*.erl
	
prepare_examples: compile
	mkdir -p ebin/examples

prepare: ebin

ebin:
	mkdir ebin

clean:
	rm -rf ./ebin
