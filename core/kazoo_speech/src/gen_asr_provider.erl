%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_asr_provider).

-include("kazoo_speech.hrl").

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Returns the preferred content type for the ASR provider.
%%% @end
%%%-----------------------------------------------------------------------------
-callback preferred_content_type() -> kz_term:ne_binary().

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Retuns a list of supported content types for the ASR provider.
%%% @end
%%%-----------------------------------------------------------------------------
-callback accepted_content_types() -> kz_term:ne_binaries().

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Execute a transcription request to the ASR provider API.
%%% @end
%%%-----------------------------------------------------------------------------
-callback freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> asr_resp().

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Execute a command list request to the ASR provider API.
%%% @end
%%%-----------------------------------------------------------------------------
-callback commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> provider_return().
