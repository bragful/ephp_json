

# ePHP JSON #

Copyright (c) 2017 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio" ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![Build Status](https://img.shields.io/travis/altenwald/ephp_json/master.svg)](https://travis-ci.org/altenwald/ephp_json)
[![Codecov](https://img.shields.io/codecov/c/github/altenwald/ephp_json.svg)](https://codecov.io/gh/altenwald/ephp_json)
[![License: LGPL 2.1](https://img.shields.io/github/license/altenwald/ephp_json.svg)](https://raw.githubusercontent.com/altenwald/ephp_json/master/COPYING)

This library implements the JSON functions as is in PHP code for [ephp](https://github.com/altenwald/ephp) keeping in mind to have it as pure 100% Erlang.


### <a name="Requirements">Requirements</a> ###

ePHP JSON requires to be run over an Erlang/OTP +R16, but not all the versions are full compatible or recommended. See the list:

| Erlang Version | Support | Notes |
|:---|:---:|:---|
| 20.0 | :heavy_check_mark: | Recommended if you use OTP 20 |
| 19.3 | :heavy_check_mark: | Recommended if you use OTP 19 |
| 19.2 | :heavy_check_mark: | |
| 19.1 | :heavy_check_mark: | |
| 19.0 | :heavy_check_mark: | |
| 18.3 | :heavy_check_mark: | Recommended if you use OTP 18 |
| 18.2.1 | :heavy_check_mark: | |
| 18.2 | :heavy_check_mark: | |
| 18.1 | :heavy_check_mark: | |
| 18.0 | :heavy_check_mark: | |
| 17.5 | :heavy_check_mark: | Recommended if you use OTP 17 |
| 17.4 | :heavy_check_mark: | |
| 17.3 | :x: | fail in SSL |
| 17.2 | :x: | no tests available in Travis-CI |
| 17.1 | :heavy_check_mark: | |
| 17.0 | :heavy_check_mark: | |
| R16B03-1 | :heavy_check_mark: | Recommended if you use OTP R16 |
| R16B03 | :heavy_check_mark: | |
| R16B02 | :heavy_check_mark: | |
| R16B01 | :x: | fails in math lib |


### <a name="Getting_Started">Getting Started</a> ###

A simple way to use, is include in your project `rebar.config` the following dependency line:

```erlang
    {ephp, ".*", {git, "git://github.com/altenwald/ephp_json.git", master}}
```

And use the following code in your project:

```erlang
{ok, Ctx} = ephp:context_new(),
PHP = "Empty array output as array: <?=json_encode([])?>",
{ok, Text} = ephp:eval(Ctx, PHP).
```

The result stored in `Text` should be:

```
Empty array output as array: []
```

Enjoy!


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/altenwald/ephp/blob/master/doc/ephp_lib_json.md" class="module">ephp_lib_json</a></td></tr></table>

