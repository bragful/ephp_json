-module(ephp_lib_json).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,

    json_encode/3
]).

-include_lib("ephp/include/ephp.hrl").
-include("php_json.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {json_encode, [pack_args, {args, [mixed, {integer, 0}, {integer, 512}]}]}
    %{json_decode, [pack_args, {args, [string, {boolean, false}, {integer, 512}, {integer, 0}]}]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [
    {<<"JSON_ERROR_NONE">>, ?JSON_ERROR_NONE},
    {<<"JSON_ERROR_DEPTH">>, ?JSON_ERROR_DEPTH},
    {<<"JSON_ERROR_STATE_MISMATCH">>, ?JSON_ERROR_STATE_MISMATCH},
    %% TODO: insert the rest of the error constants: http://php.net/manual/en/json.constants.php

    %% decode options
    {<<"JSON_BIGINT_AS_STRING">>, ?JSON_BIGINT_AS_STRING},
    {<<"JSON_OBJECT_AS_ARRAY">>, ?JSON_OBJECT_AS_ARRAY},

    %% encode options
    {<<"JSON_HEX_TAG">>, ?JSON_HEX_TAG},
    {<<"JSON_HEX_AMP">>, ?JSON_HEX_AMP},
    {<<"JSON_HEX_APOS">>, ?JSON_HEX_APOS},
    {<<"JSON_HEX_QUOT">>, ?JSON_HEX_QUOT},
    {<<"JSON_FORCE_OBJECT">>, ?JSON_FORCE_OBJECT}
    %% TODO: insert the rest of the options for encode: http://php.net/manual/en/json.constants.php
].


-spec json_encode(context(), line(), [var_value()]) -> binary().

json_encode(_Ctx, _Line, [{_, ToEncode}|_]) ->
    jsone_encode(ToEncode).

%-spec json_decode(context(), line(), [var_value()]) -> mixed().

