
all:
	@test -d _build/default/lib || rebar3 get-deps
	@./rebar3 compile

release:
	@test -d _build/default/lib || rebar3 get-deps
	@./rebar3 release

dev_release:
	@./rebar3 as dev release
	
prod_release:
	@./rebar3 as prod release

tar:
	@./rebar3 tar

run:
	@./rebar3 run

clean:
	@./rebar3 clean --all
