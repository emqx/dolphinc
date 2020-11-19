REBAR = rebar3
all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

ct:
	$(REBAR) as test ct

eunit:
	$(REBAR) as test eunit

xref:
	$(REBAR) xref

dialyzer:
	$(REBAR) dialyzer

cover:
	$(REBAR) cover

distclean:
	@rm -rf _build
