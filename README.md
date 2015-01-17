bddr
====
[![Build
Status](https://travis-ci.org/pzel/bddr.svg?branch=master)](https://travis-ci.org/pzel/bddr)

Minimalist BDD for Erlang, sans cucumbers

It's pronounced "better". 

why?
====

- To enforce strictness as to what is being tested
- To allow groups of develpers to understand each other's test cases
- To eliminate `init_per_*` and `end_per_*` anti-patterns

how?
====

Here is a basic test with independent givens:

```erlang

bddr:test([given_app_started(),
           given_user_in_db("user", "password")],

          fun([App,{User,Pass}]) ->
            when_user_logs_in(App, User, Pass) end,

          fun(Screen) ->
            "Welcome, User!" = welcome_text(Screen) end).

```

If that seems too raw to you, add some sugar in the form of macros: 

```erlang

-include_lib("bddr/include/bddr.hrl").

bddr:test(?Given() ->
            server_running() end,

          ?When(Server) ->
            i_query_server(Server, "hello") end,

          ?Then(Reply) ->
            {ok, "hello to you too!"} = Reply end,

          ?Teardown(Server) ->
             stop_server(Server) end)

```



...so it doesn't do XYZ?
========================

No, the implementation is intentionally minimal. It's the developer's
responsibility to provide his/her abstractions for the tested
components. Remember, your tests are just Erlang code. Make it readable and
extensible for others. Be humane.


-------------------------------------------------------------------------------

**Additional copyrights**

The test suite contains `dynamic_compile.erl`, copyright 2007 by Mats Cronqvist,
Chris Newcombe, and Jacob Vorreuter, licensed under the MIT License.
