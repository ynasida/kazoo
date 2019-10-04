%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author SIPLABS LLC (Ilya Ashchepkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_ring_group_tests).

-include_lib("eunit/include/eunit.hrl").

weighted_random_sort_test_() ->
    EndpointsInt = [{1, <<"ep1">>}
                   ,{2, <<"ep2">>}
                   ,{3, <<"ep3">>}
                   ],
    Endpoints = cf_ring_group:weighted_random_sort(EndpointsInt),

    ?debugFmt("~ninit: ~p~nafter: ~p~n", [EndpointsInt, Endpoints]),

    [?_assertEqual(length(EndpointsInt), length(Endpoints))
     |
     [?_assertEqual('true', lists:member(X, EndpointsInt))
      || X <- Endpoints
     ]
    ].

%% HELP-260010392
help_10392_test_() ->
    {'setup'
    ,fun setup_help_10392/0
    ,fun cleanup_help_10392/1
    ,fun(_ReturnOfSetup) ->
             help_10392()
     end
    }.

setup_help_10392() ->
    {'ok', _} = application:ensure_all_started('kazoo_bindings'),
    kz_fixturedb_util:start_me('true').

cleanup_help_10392(Pid) ->
    'ok' = application:stop('kazoo_bindings'),
    kz_fixturedb_util:stop_me(Pid).

help_10392() ->
    AccountId = <<"account0000000000000000000000001">>,
    UserId = <<"user0000000000000000000000000001">>,
    DeviceId = <<"device00000000000000000000000001">>,
    GroupId = <<"group000000000000000000000000001">>,

    Endpoint = help_10392_user_endpoint(UserId),
    User = kz_json:set_value(<<"delay">>, 0, Endpoint),
    Group = help_10392_group_endpoint(GroupId),
    EndpointDelay10 = {DeviceId
                      ,help_10392_add_source(help_10392_add_group_weight(Endpoint))
                      },
    EndpointDelay0 = {DeviceId, help_10392_add_source(User)},

    Call = kapps_call:set_account_id(AccountId, kapps_call:new()),

    Resp0 = cf_ring_group:resolve_endpoint_ids(help_10392_data([User, Group]), Call),
    Resp1 = cf_ring_group:resolve_endpoint_ids(help_10392_data([Group, User]), Call),
    Resp2 = cf_ring_group:resolve_endpoint_ids(help_10392_data([User, User]), Call),
    Resp3 = cf_ring_group:resolve_endpoint_ids(help_10392_data([Group, Group]), Call),

    Expected0 = [EndpointDelay0, EndpointDelay10],
    Expected1 = [EndpointDelay10, EndpointDelay0],
    Expected2 = [EndpointDelay0],
    Expected3 = [EndpointDelay10],

    [{"Honor RG member's delay and order: [User, Group]", ?_assertEqual(Expected0, Resp0)}
    ,{"Honor RG member's delay and order: [Group, User]", ?_assertEqual(Expected1, Resp1)}
    ,{"Honor RG member's delay and order: [User, User]", ?_assertEqual(Expected2, Resp2)}
    ,{"Honor RG member's delay and order: [Group, Group]", ?_assertEqual(Expected3, Resp3)}
    ].

help_10392_user_endpoint(UserId) ->
    kz_json:from_list([{<<"endpoint_type">>,<<"user">>}
                      ,{<<"id">>, UserId}
                      ,{<<"delay">>,10}
                      ,{<<"timeout">>,20}
                      ]).

help_10392_group_endpoint(GroupId) ->
    kz_json:from_list([{<<"endpoint_type">>, <<"group">>}
                      ,{<<"id">>, GroupId}
                      ,{<<"delay">>, 10}
                      ,{<<"timeout">>, 20}
                      ]).

help_10392_add_group_weight(Endpoint) ->
    kz_json:set_value(<<"weight">>, 20, Endpoint).

help_10392_data(TestEndpoints) ->
    kz_json:from_list([{<<"name">>, <<"RG HELP-10392">>}
                      ,{<<"endpoints">>, TestEndpoints}
                      ,{<<"strategy">>, <<"simultaneous">>}
                      ,{<<"timeout">>, 30}
                      ,{<<"repeats">>, 1}
                      ,{<<"ignore_forward">>, true}
                      ]).

help_10392_add_source(Endpoint) ->
    kz_json:set_value(<<"source">>, <<"cf_ring_group">>, Endpoint).
