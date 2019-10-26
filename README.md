

# ePHP JSON #

Copyright (c) 2017-2019 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio" ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![Build Status](https://img.shields.io/travis/bragful/ephp_json/master.svg)](https://travis-ci.org/bragful/ephp_json)
[![Codecov](https://img.shields.io/codecov/c/github/bragful/ephp_json.svg)](https://codecov.io/gh/bragful/ephp_json)
[![License: LGPL 2.1](https://img.shields.io/github/license/bragful/ephp_json.svg)](https://raw.githubusercontent.com/bragful/ephp_json/master/COPYING)

This library implements the JSON functions as is in PHP code for [ephp](https://github.com/bragful/ephp) keeping in mind to have it as pure 100% Erlang.


### <a name="Requirements">Requirements</a> ###

ePHP JSON requires to be run over an Erlang/OTP 17+, but not all the versions are full compatible or recommended. See the list:

| Erlang Version | Support | Notes |
|:---|:---:|:---|
| 22.1 | :heavy_check_mark: | Recommended if you use OTP 22 |
| 22.0 | :heavy_check_mark: | |
| 21.3 | :heavy_check_mark: | Recommended if you use OTP 21 |
| 21.2 | :heavy_check_mark: | |
| 21.1 | :heavy_check_mark: | |
| 21.0 | :heavy_check_mark: | |
| 20.3 | :x: | Fails in math and number conversion |
| 20.2 | :heavy_check_mark: | Recommended if you use OTP 20 |
| 20.1 | :heavy_check_mark: | |
| 20.0 | :heavy_check_mark: | |
| 19.3 | :heavy_check_mark: | Recommended if you use OTP 19 |
| 19.2 | :heavy_check_mark: | |
| 19.1 | :heavy_check_mark: | |
| 19.0 | :heavy_check_mark: | |
| 18.3 | :heavy_check_mark: | Recommended if you use OTP 18 |
| 18.2.1 | :heavy_check_mark: | |
| 18.2 | :heavy_check_mark: | |
| 18.1 | :heavy_check_mark: | |
| 18.0 | :heavy_check_mark: | |


### <a name="Getting_Started">Getting Started</a> ###

A simple way to use, is include in your project `rebar.config` the following dependency line:

```erlang
    {ephp_json, ".*", {git, "git://github.com/bragful/ephp_json.git", master}}
```

And use the following code in your project:

```erlang
{ok, Ctx} = ephp:context_new(),
ephp:register_module(Ctx, ephp_lib_json),
PHP = "Empty array output as array: <?=json_encode('tada')?>",
{ok, Text} = ephp:eval(Ctx, PHP).
```

The result stored in `Text` should be:

```
Empty array output as array: "tada"
```
Enjoy!


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/bragful/ephp_json/blob/master/doc/ephp_lib_json.md" class="module">ephp_lib_json</a></td></tr></table>

