all: compile

doc:
	./rebar3 as doc edown

clean-devel: clean
	-rm -rf _build

clean:
	./rebar3 clean

compile:
	./rebar3 compile

test:
	./rebar3 xref
	./rebar3 eunit
	./rebar3 cover
	./covertool \
		-cover _build/test/cover/eunit.coverdata \
		-appname ephp_json \
		-output cobertura.xml

shell:
	./rebar3 shell

.PHONY: doc test compile all shell
