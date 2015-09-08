-module(tld_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(MOD, tld).


domain_test() ->
    [ ?assertEqual(Expected, ?MOD:domain(Input)) ||
        {Expected, Input} <- [
            {undefined, <<"bad.randomtld">>},
            {<<"test.com">>, <<"test.com">>},
            {<<"second.aero">>, <<"first.second.aero">>},
            {<<"third.de">>, <<"first.second.third.de">>},
            {<<"domain.øksnes.no"/utf8>>, <<"test.domain.øksnes.no"/utf8>>},
            {<<"in.kiyose.tokyo.jp">>, <<"somewhere.in.kiyose.tokyo.jp">>},
            {<<"google.com">>, <<"https://www.google.com/?some=param">>},
            {<<"юникод.онлайн"/utf8>>, <<"http://какой-то.юникод.онлайн"/utf8>>},
            {<<"ポイント.クラウド"/utf8>>, <<"セール.ポイント.クラウド"/utf8>>}
    ] ].


suffix_test() ->
    [ ?assertEqual(Expected, ?MOD:suffix(Input)) ||
        {Expected, Input} <- [
            {undefined, <<"bad.randomtld">>},
            {<<"com">>, <<"test.com">>},
            {<<"aero">>, <<"first.second.aero">>},
            {<<"de">>, <<"first.second.third.de">>},
            {<<"øksnes.no"/utf8>>, <<"test.domain.øksnes.no"/utf8>>},
            {<<"kiyose.tokyo.jp">>, <<"somewhere.in.kiyose.tokyo.jp">>},
            {<<"com">>, <<"https://www.google.com/?some=param">>},
            {<<"онлайн"/utf8>>, <<"http://какой-то.юникод.онлайн"/utf8>>},
            {<<"クラウド"/utf8>>, <<"セール.ポイント.クラウド"/utf8>>}
    ] ].
