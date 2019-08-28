-module(kazoo_asr_tests).

-include_lib("eunit/include/eunit.hrl").
-include("kazoo_speech.hrl").

-define(ASR_PROVIDER_DEFAULT, <<"ispeech">>).

-define(ASR_ACCEPT_GOOGLE, [<<"audio/wav">>, <<"application/wav">>]).
-define(ASR_ACCEPT_ISPEECH, [<<"application/wav">>]).
-define(ASR_ACCEPT_DEFAULT, [<<"application/wav">>]).

-define(ASR_PREF_GOOGLE, <<"application/wav">>).
-define(ASR_PREF_ISPEECH, <<"application/wav">>).

%%------------------------------------------------------------------------------
%% Test system default provider through kazoo_asr abstraction
%%------------------------------------------------------------------------------
asr_default_test_() ->
    [{"kazoo_asr system default provider abstraction.", default_asr_provider_test()}
    ,{"kazoo_asr system default accepted content types test.", default_asr_accept_test()}
    ].

%%------------------------------------------------------------------------------
%% Test google asr provider through kazoo_asr abstraction
%%------------------------------------------------------------------------------
asr_google_test_() ->
    {'setup'
    ,fun mock_me/0
    ,fun cleanup/1
    ,fun(_) -> [{"kazoo_asr google provider abstraction.", google_asr_provider_test()}
               ,{"kazoo_asr google accepted content types test.", google_asr_accept_test()}
               ]
     end
    }.

%%------------------------------------------------------------------------------
%% Test ispeech provider through kazoo_asr abstraction
%%------------------------------------------------------------------------------
asr_ispeech_test_() ->
    {'setup'
    ,fun mock_me/0
    ,fun cleanup/1
    ,fun(_) -> [{"kazoo_asr ispeech provider abstraction.", ispeech_asr_provider_test()}
               ,{"kazoo_asr ispeech accepted content types test.", ispeech_asr_accept_test()}
               ]
     end
    }.

%%------------------------------------------------------------------------------
%% Intialize test fixtures
%%------------------------------------------------------------------------------
mock_me() -> meck:new('kapps_config', [unstick]).

%%------------------------------------------------------------------------------
%% Teardown fixtures
%%------------------------------------------------------------------------------
cleanup(_) -> meck:unload().

%%------------------------------------------------------------------------------
%%  Mock google kapps_config calls
%%------------------------------------------------------------------------------
config_asr_google(_, <<"asr_provider">>, _) -> <<"google">>;
config_asr_google(_, <<"asr_preferred_content_type">>, _) -> ?ASR_PREF_GOOGLE.

%%------------------------------------------------------------------------------
%% Mock ispeech kapps_config calls
%%------------------------------------------------------------------------------
config_asr_ispeech(_, <<"asr_provider">>, _) -> <<"ispeech">>;
config_asr_ispeech(_, <<"asr_preferred_content_type">>, _) -> ?ASR_PREF_ISPEECH.

%%------------------------------------------------------------------------------
%% Test fixtures
%%------------------------------------------------------------------------------
default_asr_provider_test() ->
    [{"Checking system default ASR provider"
    ,?_assertEqual(kazoo_asr:default_provider(), ?ASR_PROVIDER_DEFAULT)}
    ].

default_asr_accept_test() ->
    [{"Checking system default accepted content type"
    ,?_assertEqual(kazoo_asr:accepted_content_types(), ?ASR_ACCEPT_DEFAULT)}
    ].

google_asr_provider_test() ->
    meck:expect('kapps_config', 'get_ne_binary', fun config_asr_google/3),
    [{"Checking google is default ASR"
    ,?_assertEqual(kazoo_asr:default_provider(), <<"google">>)}
    ].

google_asr_accept_test() ->
    meck:expect('kapps_config', 'get_ne_binary', fun config_asr_google/3),
    [{"Checking google accepted content types"
    ,?_assertEqual(kazoo_asr:accepted_content_types(), ?ASR_ACCEPT_GOOGLE)}
    ].

ispeech_asr_provider_test() ->
    meck:expect('kapps_config', 'get_ne_binary', fun config_asr_ispeech/3),
    [{"Checking ispeech is default ASR"
    ,?_assertEqual(kazoo_asr:default_provider(), <<"ispeech">>)}
    ].

ispeech_asr_accept_test() ->
    meck:expect('kapps_config', 'get_ne_binary', fun config_asr_ispeech/3),
    [{"Checking ispeech accepted content types"
    ,?_assertEqual(kazoo_asr:accepted_content_types(), ?ASR_ACCEPT_ISPEECH)}
    ].
