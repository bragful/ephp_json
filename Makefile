all: compile

doc:
	./rebar3 as doc edown

clean-devel: clean
	-rm -rf _build

clean:
	-rm -f .build_date
	./rebar3 clean

compile:
	./rebar3 compile

test:
	./rebar3 do xref, eunit, cover, covertool generate
	mv _build/test/covertool/ephp_json.covertool.xml cobertura.xml

shell:
	./rebar3 shell

ephp: compile
	./rebar3 escriptize
	cp -f _build/default/bin/ephp ephp
	-rm -f .build_date

.PHONY: doc test compile all shell
