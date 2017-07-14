-module(ephp_lib_json_tests).

-include_lib("eunit/include/eunit.hrl").


tada_test() ->
    {ok, Ctx} = ephp:context_new(),
    ephp:register_module(Ctx, ephp_lib_json),
    PHP = "Empty array output as array: <?=json_encode('tada')?>",
    Expected = <<"Empty array output as array: \"tada\"">>,

    {ok, Output} = ephp_output:start_link(Ctx, false),
    ephp_context:set_output_handler(Ctx, Output),
    ?assertEqual({ok, false}, ephp:eval(Ctx, PHP)),
    Out = ephp_context:get_output(Ctx),
    ephp_context:destroy_all(Ctx),
    ?assertEqual(Expected, Out).

