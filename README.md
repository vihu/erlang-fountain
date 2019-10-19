erlang_fountain
=====

![Build status](https://github.com/vihu/erlang-fountain/workflows/Erlang%20CI/badge.svg)

Erlang bindings to fountain codes written in rust

Build
-----

    $ make

Test
-----

    $ make test

Example
-----

```
1> B = crypto:strong_rand_bytes(1024).
<<230,47,221,249,20,111,59,135,167,61,36,191,15,82,139,96,
  239,41,255,123,198,115,118,44,101,233,47,132,181,...>>
2> {ok, E} = erlang_fountain:encode_systematic(B, 256).
{ok,#Ref<0.4148393654.540409859.180370>}
3> erlang_fountain:decode(E, 1024, 256).
{ok,<<230,47,221,249,20,111,59,135,167,61,36,191,15,82,
      139,96,239,41,255,123,198,115,118,44,101,233,47,...>>}
4> {ok, E2} = erlang_fountain:encode_random(B, 256).
{ok,#Ref<0.4148393654.540409859.180383>}
5> erlang_fountain:decode(E2, 1024, 256).
{ok,<<230,47,221,249,20,111,59,135,167,61,36,191,15,82,
      139,96,239,41,255,123,198,115,118,44,101,233,47,...>>}
```
