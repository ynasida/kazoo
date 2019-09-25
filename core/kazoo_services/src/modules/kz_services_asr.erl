%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_asr).

-export([fetch/1
        ,flat_rate/1, flat_rate/2
        ,quantities/1
        ]).

-include("services.hrl").

-define(LEDGER_KEY, <<"ledgers">>).
-define(ASR_LEDGER_KEY, <<"asr">>).
-define(ASR_VIEW, <<"ledgers/summary_by_account">>).
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_services:services() | kz_term:ne_binary()) -> kz_json:object().
fetch(?NE_BINARY=AccountId) ->
    FetchOptions = ['hydrate_plans'],
    fetch(kz_services:fetch(AccountId, FetchOptions));
fetch(Services) ->
   ASRDict = kz_services_plans:foldl(fun fetch_foldl/3
                                      ,dict:new()
                                      ,kz_services:plans(Services)
                                      ),
    kz_json:from_list(dict:to_list(ASRDict)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_foldl(kz_term:ne_binary(), kz_services_plans:plans_list(), dict:dict()) -> dict:dict().
fetch_foldl(_BookkeeperHash, [], Providers) ->
    Providers;
fetch_foldl(_BookkeeperHash, PlansList, Providers) ->
    Plan = kz_services_plans:merge(PlansList),
    kz_json:foldl(fun(K, V, A) ->
                          dict:store(K, V, A)
                  end
                 ,Providers
                 ,kz_services_plan:asr(Plan)
                 ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flat_rate(kz_term:ne_binary()) -> kz_currency:dollars().
flat_rate(AccountId) ->
    flat_rate(AccountId, kazoo_asr:default_provider()).

-spec flat_rate(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_currency:dollars().
flat_rate(AccountId, Provider) ->
    Items = fetch(AccountId),
    kz_json:get_number_value([Provider, <<"rate">>], Items, 0).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec quantities(kz_term:ne_binary()) -> kz_json:object().
quantities(AccountId) ->
    MODB = kazoo_modb:get_modb(AccountId),
    ViewOptions = [{'reduce', 'true'}
                  ],
    case kz_datamgr:get_results(MODB, ?ASR_VIEW, ViewOptions) of
        {'ok', Sources} -> generate_summary(Sources);
        {'error', _} ->
            lager:error("Error retrieving asr quantities"),
            kz_json:new()
    end.

-spec generate_summary(kz_json:terms()) -> kz_json:object().
generate_summary(JObj) ->
    Usage = kz_json:get_value([<<"1">>, <<"value">>, ?LEDGER_KEY, ?ASR_LEDGER_KEY, <<"usage">>], JObj, kz_json:new()),

    P = kz_json:get_ne_value(<<"type">>, Usage),
    Q = kz_json:get_number_value(<<"quantity">>, Usage),
    ProviderStats = kz_json:from_list([{P, Q}]),
    kz_json:from_list_recursive([{<<"asr">>, ProviderStats}]).


