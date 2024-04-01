-module(holster_gun_event_h).

-include_lib("kernel/include/logger.hrl").

-behavior(gun_event).

-export([
	init/2,
	domain_lookup_start/2,
	domain_lookup_end/2,
	connect_start/2,
	connect_end/2,
	tls_handshake_start/2,
	tls_handshake_end/2,
	request_start/2,
	request_headers/2,
	request_end/2,
	push_promise_start/2,
	push_promise_end/2,
	response_start/2,
	response_inform/2,
	response_headers/2,
	response_trailers/2,
	response_end/2,
	ws_upgrade/2,
	ws_recv_frame_start/2,
	ws_recv_frame_header/2,
	ws_recv_frame_end/2,
	ws_send_frame_start/2,
	ws_send_frame_end/2,
	protocol_changed/2,
	origin_changed/2,
	cancel/2,
	disconnect/2,
	terminate/2
]).

init(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

domain_lookup_start(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

domain_lookup_end(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

connect_start(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

connect_end(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

tls_handshake_start(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

tls_handshake_end(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

request_start(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

request_headers(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

request_end(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

push_promise_start(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

push_promise_end(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

response_start(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

response_inform(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

response_headers(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

response_trailers(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

response_end(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

ws_upgrade(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

ws_recv_frame_start(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

ws_recv_frame_header(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

ws_recv_frame_end(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

ws_send_frame_start(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

ws_send_frame_end(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

protocol_changed(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

origin_changed(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

cancel(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

disconnect(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.

terminate(EventData, State) ->
	?LOG_INFO(#{ event_data => EventData, state => State }),
	State.
