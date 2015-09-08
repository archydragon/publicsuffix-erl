publicsuffix-erl
================

Erlang interface to the [Public Suffix List](https://publicsuffix.org/).

The Public Suffix List (PSL) is a set of rules describing "effective top-level domains" and can be used to determine the registered domain for a given host name.

Inspired by [publicsuffix_erlang](https://github.com/sinkovsky/publicsuffix_erlang) (but supports unicode TLDs too) and [tld](https://github.com/barseghyanartur/tld).


Requirements
------------

    * Erlang 17.0 or newer (should work on R16 too but hasn't been tested)
    * GNU make


Installation
------------

```
git clone https://github.com/Mendor/publicsuffix-erl.git
cd publicsuffix-erl
make
```


Usage
-----

```erlang
[inori] ~/dev/my/publicsuffix-erl % erl +pc unicode -pa ebin
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.3  (abort with ^G)
1> tld:domain(<<"some.site.co.uk">>).
<<"site.co.uk">>
2> tld:domain(<<"тут.какой-то.сайт"/utf8>>).   % unicode domains work!
<<"какой-то.сайт"/utf8>>
3> tld:domain(<<"https://www.google.com/">>).  % and extracting domain from the whole URI
<<"google.com">>
4> tld:suffix(<<"ポイント.ストア"/utf8>>).       % and if you need only the suffix
<<"ストア"/utf8>>
```


TODO
----

    1. Rebar support.
    2. Punycode support.


License
-------

[MIT](http://opensource.org/licenses/mit-license.html)
