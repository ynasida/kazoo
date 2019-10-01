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
    UserId = <<"help-10392-user-id">>,
    DevId = <<"help-10392-device-id">>,
    GroupId = <<"help-10392-group-id">>,

    meck:new('kz_datamgr', ['passthrough']),
    meck:new('kz_attributes', ['passthrough']),
    F = fun(_, _) -> help_10392_open_cache_doc(UserId) end,
    meck:expect('kz_datamgr', 'open_cache_doc', F),
    meck:expect('kz_attributes', 'owned_by', fun(_, _, _) -> [DevId] end),

    Endpoint = help_10392_endpoint(UserId),
    User = kz_json:set_value(<<"delay">>, 0, Endpoint),
    Group = help_10392_group(GroupId),
    EndDelay10 = {DevId, help_10392_add_source(help_10392_add_group_weight(Endpoint))},
    EndDelay0 = {DevId, help_10392_add_source(User)},

    Data0 = help_10392_data([User, Group]),
    Data1 = help_10392_data([Group, User]),
    Data2 = help_10392_data([User, User]),
    Data3 = help_10392_data([Group, Group]),
    Expected0 = [EndDelay0, EndDelay10],
    Expected1 = [EndDelay10, EndDelay0],
    Expected2 = [EndDelay0],
    Expected3 = [EndDelay10],

    Resp0 = cf_ring_group:resolve_endpoint_ids(Data0, kapps_call:new()),
    Resp1 = cf_ring_group:resolve_endpoint_ids(Data1, kapps_call:new()),
    Resp2 = cf_ring_group:resolve_endpoint_ids(Data2, kapps_call:new()),
    Resp3 = cf_ring_group:resolve_endpoint_ids(Data3, kapps_call:new()),
    meck:unload(),

    [{"Honor RG member's delay and order: [User, Group]", ?_assertEqual(Expected0, Resp0)}
    ,{"Honor RG member's delay and order: [Group, User]", ?_assertEqual(Expected1, Resp1)}
    ,{"Honor RG member's delay and order: [User, User]", ?_assertEqual(Expected2, Resp2)}
    ,{"Honor RG member's delay and order: [Group, Group]", ?_assertEqual(Expected3, Resp3)}
    ].

help_10392_endpoint(UserId) ->
    kz_json:from_list([{<<"endpoint_type">>,<<"user">>}
                      ,{<<"id">>, UserId}
                      ,{<<"delay">>,10}
                      ,{<<"timeout">>,20}
                      ]).

help_10392_add_group_weight(Endpoint) ->
    kz_json:set_value(<<"weight">>, 20, Endpoint).

help_10392_group(GroupId) ->
    kz_json:from_list([{<<"endpoint_type">>, <<"group">>}
                      ,{<<"id">>, GroupId}
                      ,{<<"delay">>, 10}
                      ,{<<"timeout">>, 20}
                      ]).

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

%% Since the functions that process the returned value from this function
%% (cf_ring_group:[unordered_group_members/3, order_group_members/3]) only need the
%% `endpoints' key's value, it is returning a json with only that key, instead of a full
%% `group' object.
help_10392_open_cache_doc(UserId) ->
    Endpoints = [{UserId, [{<<"type">>, <<"user">>}, {<<"weight">>, 1}]}],
    {ok, kz_json:from_list_recursive([{<<"endpoints">>, Endpoints}])}.
