

# Module tempdir #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2014 Finexkap

Erlang module for managing temporary files

__Authors:__ Gregoire Lejeune ([`gl@finexkap.com`](mailto:gl@finexkap.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-tmpname_option">tmpname_option()</a> ###


<pre><code>
tmpname_option() = {prefix, string()} | {path, string()}
</code></pre>




### <a name="type-tmpname_options">tmpname_options()</a> ###


<pre><code>
tmpname_options() = [<a href="#type-tmpname_option">tmpname_option()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#mktmp-1">mktmp/1</a></td><td>Equivalent to <a href="#mktmp-2"><tt>mktmp([], Fun)</tt></a>.</td></tr><tr><td valign="top"><a href="#mktmp-2">mktmp/2</a></td><td> 
Create a temporary directory.</td></tr><tr><td valign="top"><a href="#name-0">name/0</a></td><td>Equivalent to <a href="#name-1"><tt>name([])</tt></a>.</td></tr><tr><td valign="top"><a href="#name-1">name/1</a></td><td> 
Get a temporary dir name.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="mktmp-1"></a>

### mktmp/1 ###

`mktmp(Fun) -> any()`

Equivalent to [`mktmp([], Fun)`](#mktmp-2).

<a name="mktmp-2"></a>

### mktmp/2 ###

`mktmp(Options, Fun) -> any()`


Create a temporary directory

Options:

* `prefix` : temporary directory prefix (default: `tmp_`
* `path` : temp file path (default: `ostemp:dir()`)
* `remove` : remove the temp dir (default: `true`)

<a name="name-0"></a>

### name/0 ###

`name() -> any()`

Equivalent to [`name([])`](#name-1).

<a name="name-1"></a>

### name/1 ###

<pre><code>
name(Options::<a href="#type-tmpname_options">tmpname_options()</a>) -&gt; string()
</code></pre>
<br />


Get a temporary dir name

Options:

* `prefix` : temporary directory prefix (default: `tmp_`
* `path` : temp file path (default: `ostemp:dir()`)

