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
-include("ephp_json.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {json_encode, [pack_args, {args, [mixed, {integer, 0}, {integer, 512}]}]}
    % {json_decode, [
    %     pack_args,
    %     {args, [string, {boolean, false}, {integer, 512}, {integer, 0}]}]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [
    {<<"JSON_ERROR_NONE">>, ?JSON_ERROR_NONE},
    {<<"JSON_ERROR_DEPTH">>, ?JSON_ERROR_DEPTH},
    {<<"JSON_ERROR_STATE_MISMATCH">>, ?JSON_ERROR_STATE_MISMATCH},
    {<<"JSON_ERROR_CTRL_CHAR">>, ?JSON_ERROR_CTRL_CHAR},
    {<<"JSON_ERROR_SYNTAX">>, ?JSON_ERROR_SYNTAX},
    {<<"JSON_ERROR_UTF8">>, ?JSON_ERROR_UTF8},
    {<<"JSON_ERROR_RECURSION">>, ?JSON_ERROR_RECURSION},
    {<<"JSON_ERROR_INF_OR_NAN">>, ?JSON_ERROR_INF_OR_NAN},
    {<<"JSON_ERROR_UNSUPPORTED_TYPE">>, ?JSON_ERROR_UNSUPPORTED_TYPE},

    %% decode options
    {<<"JSON_BIGINT_AS_STRING">>, ?JSON_BIGINT_AS_STRING},
    {<<"JSON_OBJECT_AS_ARRAY">>, ?JSON_OBJECT_AS_ARRAY},

    %% encode options
    {<<"JSON_HEX_TAG">>, ?JSON_HEX_TAG},
    {<<"JSON_HEX_AMP">>, ?JSON_HEX_AMP},
    {<<"JSON_HEX_APOS">>, ?JSON_HEX_APOS},
    {<<"JSON_HEX_QUOT">>, ?JSON_HEX_QUOT},
    {<<"JSON_FORCE_OBJECT">>, ?JSON_FORCE_OBJECT},
    {<<"JSON_NUMERIC_CHECK">>, ?JSON_NUMERIC_CHECK},
    {<<"JSON_UNESCAPED_SLASHES">>, ?JSON_UNESCAPED_SLASHES},
    {<<"JSON_PRETTY_PRINT">>, ?JSON_PRETTY_PRINT},
    {<<"JSON_UNESCAPED_UNICODE">>, ?JSON_UNESCAPED_UNICODE},
    {<<"JSON_PARTIAL_OUTPUT_ON_ERROR">>, ?JSON_PARTIAL_OUTPUT_ON_ERROR}
].


-spec json_encode(context(), line(), [var_value()]) -> binary().

json_encode(_Ctx, _Line, [{_, ToEncode}, {_, Flags}|_]) ->
    Opts = flags_to_opts(Flags),
    jsone:encode(filter_data(ToEncode, Flags), Opts).

%-spec json_decode(context(), line(), [var_value()]) -> mixed().

flag(Flags, {Flag, Option}) when Flags band Flag > 0 -> [Option];
flag(_, _) -> [].

flags_to_opts(Flags) ->
    Checks = [
        {?JSON_HEX_TAG, hex_tag},
        {?JSON_HEX_QUOT, hex_quote},
        {?JSON_HEX_APOS, hex_apos},
        {?JSON_HEX_AMP, hex_amp},
        {?JSON_UNESCAPED_UNICODE, native_utf8}
    ],
    ParseFlag = fun(X) -> flag(Flags, X) end,
    lists:flatmap(ParseFlag, Checks).

-type plain_data() :: integer() | float() | binary() | boolean() | undefined.

-spec filter_data(mixed(), pos_integer()) -> proplists:proplists() |
                                             plain_data() |
                                             [plain_data()].
%@doc ensure the data to be converted to JSON is correct.
%@end
filter_data(Array, Flags) when ?IS_ARRAY(Array) ->
    translate_hash(lists:map(fun({K, V}) ->
        {K, filter_data(V, Flags)}
    end, ephp_array:to_list(Array)), Flags);
filter_data(Value, _Flags) ->
    Value.

-spec translate_hash(proplists:proplists(),
                     Flags :: pos_integer()) -> [plain_data()] |
                                                proplists:proplists().
%@doc if all of the keys are numeric (integers) the data is translated as array
%     else is returned as is.
%@end
translate_hash([], Flags) when Flags band ?JSON_FORCE_OBJECT > 0 ->
    [{}];
translate_hash(HoA, Flags) ->
    case lists:all(fun({K,_}) -> is_integer(K) end, HoA) of
        true when Flags band ?JSON_FORCE_OBJECT > 0 ->
            [ {ephp_data:to_bin(K), V} || {K, V} <- HoA ];
        true ->
            [ V || {_,V} <- HoA ];
        false ->
            HoA
    end.
