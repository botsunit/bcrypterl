REBAR = ./rebar

.PHONY: compile get-deps test doc

all: compile doc

compile: get-deps
	@$(REBAR) compile

get-deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

realclean: clean
	@$(REBAR) delete-deps

test: compile
	@mkdir -p .eunit/data
	@cp -r test/data/* .eunit/data
	@$(REBAR) skip_deps=true eunit

doc:
	@rm -f documentation.md
	@rm -rf doc
	@./make_doc

dev: compile
	@erl -pa ebin include deps/*/ebin deps/*/include #-config config/samsa.config

analyze: checkplt
	@$(REBAR) skip_deps=true dialyze

buildplt:
	@$(REBAR) skip_deps=true build-plt

checkplt: buildplt
	@$(REBAR) skip_deps=true check-plt
