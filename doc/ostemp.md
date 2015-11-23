

# Module ostemp #
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2014 Finexkap

Erlang module for managing temporary files

__Authors:__ Gregoire Lejeune ([`gl@finexkap.com`](mailto:gl@finexkap.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dir-0">dir/0</a></td><td> 
Returns a writable temporary directory.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dir-0"></a>

### dir/0 ###

<pre><code>
dir() -&gt; string() | false
</code></pre>
<br />


Returns a writable temporary directory.

Searches for directories in the following order:

1. the directory named by the `TMPDIR` environment variable
2. the directory named by the `TEMP` environment variable
3. the directory named by the `TMP` environment variable
4. `C:\TMP` on Windows or `/tmp` on Unix 
5. as a last resort, the current working directory

Returns `false` if none of the above are writable

