-module(holster_basic_auth_webserver).

-include_lib("kernel/include/logger.hrl").

-export([
	init/2,
	content_types_provided/2,
	is_authorized/2,
	to_text/2
]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
	io:format("~p\n", [#{ headers => cowboy_req:headers(Req) }]),
	case cowboy_req:parse_header(<<"authorization">>, Req) of
		X = {basic, User = <<"Alladin">>, <<"open_sesame">>} ->
			?LOG_NOTICE(#{basic_auth_header => X}),
			{true, Req, User};
		X ->
			?LOG_NOTICE(#{basic_auth_header => X}),
			{{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
	end.

content_types_provided(Req, State) ->
	{[
		{<<"text/plain">>, to_text}
	], Req, State}.

to_text(Req, User) ->
	{<< "Hello, ", User/binary, "!\n" >>, Req, User}.