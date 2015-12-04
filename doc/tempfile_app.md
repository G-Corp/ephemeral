

# Module tempfile_app #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

tempfile should have been started before using functions that generate
random strings (the ones using temp_utils:randstr/1), as initialization
reset the random generator seed, in order to guarantee a good distribution
of random strings.

__Behaviours:__ [`application`](application.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-2"></a>

### start/2 ###

`start(Type, Args) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(State) -> any()`

