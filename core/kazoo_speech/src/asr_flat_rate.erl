-module(asr_flat_rate).

-include("kazoo_speech.hrl").

-export([authorize/1
        ,debit/1
        ]).

-define(ASR_LEDGER, <<"ASR Transcriptions">>).
-define(ASR_PROVIDER, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<"ispeech">>)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(asr_req()) -> asr_req().
authorize(Request) ->
    lager:notice("Authorizing ASR Request"),
    Request#asr_req{account_authorized='true', reseller_authorized='true', impact_reseller='true'}.

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec debit(asr_req()) -> no_return().
debit(#asr_req{account_authorized='true', impact_reseller='false'}=Request) ->
    maybe_debit(Request);
debit(#asr_req{account_authorized='true', reseller_authorized='true', impact_reseller='true'}=Request) ->
    maybe_debit(Request);
debit(Request) ->
    asr_request:add_error(Request, {'error', 'asr_provider_failure', <<"unauthorized">>}).

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
%-spec maybe_credit_available(asr_req()) -> asr_req().
%maybe_credit_available(#asr_req{account_id=AccountId}=Request) ->
%    kz_currency:available_dollars(AccountId, 0),
%    Request.

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-spec maybe_debit(asr_req()) -> asr_req().
maybe_debit(#asr_req{account_authorized='true', impact_reseller='false'}=Request) ->
    lager:info("Don't need to bill reseller creating ledger entry."),
    create_ledger_usage(Request);
maybe_debit(#asr_req{account_authorized='true', reseller_authorized='true'}=Request) ->
    Accounts = [asr_request:account_id(Request)
               ,asr_request:reseller_id(Request)
               ],
    lager:notice("impact reseller true updating accounts ~p",[Accounts]),
    lists:foldl(fun create_ledger_usage/2, Request, Accounts);
maybe_debit(#asr_req{account_id=AccountId}=Request) ->
    lager:info("not enough credit in account ~s skipping transcription", [AccountId]),
    asr_request:add_error(Request, {'error', 'insufficient_funds'}).

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec create_ledger_usage(asr_req()) -> {'ok', kz_term:ne_binary()} | {'error', any()}.
create_ledger_usage(#asr_req{account_id=AccountId}=Request) ->
    lager:info("creating ledger entry."),
    create_ledger_usage(Request, AccountId).

-spec create_ledger_usage(asr_req(), kz_term:ne_binary()) -> no_return().
create_ledger_usage(AccountId, Request) ->
    Setters =
        props:filter_empty(
          [{fun kz_ledger:set_account/2, asr_request:account_id(Request)}
          ,{fun kz_ledger:set_source_service/2, ?ASR_LEDGER}
          ,{fun kz_ledger:set_source_id/2, asr_request:call_id(Request)}
          ,{fun kz_ledger:set_description/2, asr_request:description(Request)}
          ,{fun kz_ledger:set_usage_type/2, <<"conversion">>}
          ,{fun kz_ledger:set_usage_quantity/2, 1}
          ,{fun kz_ledger:set_usage_unit/2, <<"transcription">>}
          ,{fun kz_ledger:set_period_start/2, asr_request:timestamp(Request)}
          ,{fun kz_ledger:set_metadata/2, metadata(Request)}
          %,{fun kz_ledger:set_unit_amount/2, Amount}
          ]
         ),
    case kz_ledger:debit(kz_ledger:setters(Setters), AccountId) of
        {'ok', _} -> Request;
        {'error', Msg} -> asr_req:add_error(Request, {'error', 'asr_provider_failure', Msg})
    end.

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec metadata(asr_req()) -> kz_json:object().
metadata(Request) ->
    kz_json:from_list([{'recording_secs', asr_request:recording_seconds(Request)}
                      ,{'call_id', asr_request:call_id(Request)}
                      ,{'media_id', asr_request:media_id(Request)}
                      ]).
