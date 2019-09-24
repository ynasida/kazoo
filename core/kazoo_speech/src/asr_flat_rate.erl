
%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(asr_flat_rate).

-include("kazoo_speech.hrl").

-export([authorize/1
        ,debit/1
        ]).

-define(ASR_LEDGER, <<"asr">>).

%%------------------------------------------------------------------------------
%% @doc
%% Authorize the ASR request has sufficient funds
%% @end
%%------------------------------------------------------------------------------
-spec authorize(asr_req()) -> asr_req().
authorize(#asr_req{account_id=AccountId, asr_provider=Provider, impact_reseller='false'}=Request) ->
    Services = kz_services:fetch(AccountId),
    Amount = kz_services_asr:flat_rate(AccountId, Provider),

    case maybe_consume_flat_rate(Services, Amount) of
        {'true', _} ->
            %_ = update_services(Services, CurrentJObj, asr_request:service_obj(Request)),
            Request#asr_req{account_authorized='true', amount=Amount};
        {'false', _} -> asr_request:add_error(Request, {'error', 'insufficient_funds'})
    end;
authorize(#asr_req{account_id=AccountId, asr_provider=Provider, impact_reseller='true', reseller_id=ResellerId}=Request) ->
    Account = kz_services:fetch(AccountId),
    Reseller = kz_services:fetch(ResellerId),
    Amount = kz_services_asr:flat_rate(AccountId, Provider),

    case {maybe_consume_flat_rate(Account, Amount), maybe_consume_flat_rate(Reseller, Amount)} of
        {{'true', _}, {'true', _}} ->
            Request#asr_req{account_authorized='true', reseller_authorized='true', amount=Amount};
        {{'true', _}, {'false', _}} ->
            Request#asr_req{account_authorized='true', reseller_authorized='false', amount=Amount};
        {{'false', _}, {'true', _}} ->
            Request#asr_req{account_authorized='false', reseller_authorized='true', amount=Amount};
        {_,_} ->
            Request#asr_req{account_authorized='false', reseller_authorized='false', amount=Amount}
    end.


%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec debit(asr_req()) -> asr_req().
debit(#asr_req{account_authorized='true', impact_reseller='false'}=Request) ->
    maybe_debit(Request);
debit(#asr_req{account_authorized='true', reseller_authorized='true', impact_reseller='true'}=Request) ->
    maybe_debit(Request);
debit(Request) ->
    asr_request:add_error(Request, {'error', 'asr_provider_failure', <<"unauthorized">>}).

%update_service_quantities(Request) ->
%    Quantities = kz_services_asr:quantities(AccountId),
%    lager:notice("updating service quantites ~p", [Quantities]),
%    kzd_services:set_account_quantities(kz_services_asr:fetch(AccountId), Quantities),

%%%------------------------------------------------------------------------------
%%% @doc
%%% Verify the ASR request has sufficient funds
%%% @end
%%%------------------------------------------------------------------------------
%-spec maybe_credit_available(asr_req()) -> asr_req().
%maybe_credit_available(#asr_req{account_id=AccountId, asr_provider=Provider}=Request) ->
%    Rate = kz_services_asr:flat_rate(AccountId, Provider),
%    lager:notice("Using Rate: ~p~n", [Rate]),
%    case kz_currency:available_dollars(AccountId, 0) of
%        {'error', _Msg} -> asr_request:add_error(Request, {'error', 'insufficient_funds'});
%         Dollars -> maybe_update_services(Request, Dollars, Rate)
%    end.

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
%-spec maybe_consume_flat_rate(asr_req(), kz_currency:dollars(), kz_currency:dollars()) -> asr_req().
%maybe_consume_flat_rate(#asr_req{impact_reseller='false'}=Request, Dollars, Rate) ->
%    case kz_services_standing:(Dollars, Rate) of
%      'false' ->
%            asr_request:add_error(Request, {'error', 'insufficient_funds'});
%        'true' ->
%            Request#asr_req{account_authorized='true', amount=Rate}
%    end;
%maybe_consume_flat_rate(#asr_req{account_id=_AccountId, reseller_id=ResellerId, impact_reseller='true'}=Request, Dollars, Rate) ->
%    ResellerDollars = kz_currency:available_dollars(ResellerId, 0),
%    case {has_enough_credit(Dollars, Rate), has_enough_credit(ResellerDollars, Rate)} of
%        {'true', 'true'} ->
%            Request#asr_req{account_authorized='true', reseller_authorized='true', amount=Rate};
%        {_, _} ->
%            asr_request:add_error(Request, {'error', 'insufficient_funds'})
%    end.

%%-spec has_enough_credit(kz_currency:dollars(), kz_currency:dollars()) -> boolean().
%%has_enough_credit(Credit, Debit) ->
%%  Credit - Debit >= 0.

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
%-spec maybe_update_services(asr_req()) -> asr_req().
%maybe_update_services(#asr_req{account_id=AccountId, asr_provider=Provider}=Request) ->

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
%-spec update_services(kz_services:service(), kz_json:object(), kz_json:object()) -> boolean().
%update_services(Services, CurrentJObj, ProposedJObj) ->
%   lager:notice("~nCurrent: ~p~n, Proposed:~p~n", [CurrentJObj, ProposedJObj]),
%    kazoo_services:commit_updates(Services
%                                 ,kz_services:to_billables(CurrentJObj)
%                                 ,kz_services:to_billables(ProposedJObj)
%                                 ).

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec maybe_consume_flat_rate(kz_services:services(), kz_currency:dollars()) -> kz_services_standing:acceptable_return().
maybe_consume_flat_rate(Services, Amount) ->
    Options = #{amount => kz_currency:dollars_to_units(Amount)},
    kz_services_standing:acceptable(Services, Options).

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-spec maybe_debit(asr_req()) -> asr_req().
maybe_debit(#asr_req{account_authorized='true', impact_reseller='false'}=Request) ->
    lager:debug("impact reseller is false"),
    create_ledger_usage(Request);
maybe_debit(#asr_req{account_id=AccountId, reseller_id=ResellerId, account_authorized='true', reseller_authorized='true'}=Request) ->
    lager:debug("impact reseller is true updating accounts [~s, ~s]", [AccountId, ResellerId]),
    lists:foldl(fun create_ledger_usage/2, Request, [AccountId, ResellerId]);
maybe_debit(Request) -> Request.

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec create_ledger_usage(asr_req()) -> asr_req().
create_ledger_usage(#asr_req{account_id=AccountId}=Request) ->
    lager:info("creating ledger entry."),
    create_ledger_usage(Request, AccountId).

-spec create_ledger_usage(asr_req(), kz_term:ne_binary()) -> asr_req().
create_ledger_usage(Request, AccountId) ->
    lager:notice("REQUEST: ~p~n", [Request]),
    Setters =
        props:filter_empty(
          [{fun kz_ledger:set_account/2, asr_request:account_id(Request)}
          ,{fun kz_ledger:set_source_service/2, ?ASR_LEDGER}
          ,{fun kz_ledger:set_source_id/2, asr_request:call_id(Request)}
          ,{fun kz_ledger:set_description/2, asr_request:description(Request)}
          ,{fun kz_ledger:set_usage_type/2, asr_request:asr_provider(Request)}
          ,{fun kz_ledger:set_usage_quantity/2, 1}
          ,{fun kz_ledger:set_usage_unit/2, <<"transcription">>}
          ,{fun kz_ledger:set_period_start/2, asr_request:timestamp(Request)}
          ,{fun kz_ledger:set_metadata/2, metadata(Request)}
          ,{fun kz_ledger:set_dollar_amount/2, asr_request:amount(Request)}
          ]
         ),
    case kz_ledger:debit(kz_ledger:setters(Setters), AccountId) of
        {'ok', _} ->
            %update_service_quantities(AccountId),
            Request;
        {'error', Msg} -> asr_request:add_error(Request, {'error', 'asr_provider_failure', kz_term:to_binary(Msg)})
    end.

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec metadata(asr_req()) -> kz_json:object().
metadata(Request) ->
    kz_json:from_list([{<<"account_id">>, asr_request:account_id(Request)}
                      ,{<<"call_id">>, asr_request:call_id(Request)}
                      ,{<<"media_id">>, asr_request:media_id(Request)}
                      ,{<<"provider">>, asr_request:asr_provider(Request)}
                      ,{<<"recording_seconds">>, asr_request:recording_seconds(Request)}
                      ]).
