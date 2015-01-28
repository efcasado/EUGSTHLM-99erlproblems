%%%========================================================================
%%% File: lists2.erl
%%%
%%% This module implements the problems 1 to 10 from the list of 99 Haskell
%%% problems.
%%%
%%% A basic EUnit test suite is provided. Note that the test suite is not
%%% injected to the lists2 module by default. If you want the test suite
%%% to be injected, you must define the TEST macro a compile time. This
%%% can be done adding -DTEST when uing erlc.
%%%
%%%    erlc -DTEST lists2.erl
%%%
%%% If you injected the test suite, you can easily validate your solution
%%% by running
%%%
%%%     erl -s eunit test lists2 -s erlang halt
%%%
%%%
%%% Author: Enrique Fernandez <enrique.fernandez (at) erlang-solutions.com>
%%% Date:   January, 2015
%%%
%%%-- LICENSE -------------------------------------------------------------
%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2015 Enrique Fernandez
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining
%%% a copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software,
%%% and to permit persons to whom the Software is furnished to do so,
%%% subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%========================================================================
-module(lists2).

-compile({no_auto_import, [length/1]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([last/1, but_last/1, element_at/2,
         length/1, reverse/1, is_palindrome/1,
         flatten/1, compress/1, pack/1, encode/1]).


%% ========================================================================
%%  API
%% ========================================================================

%%-------------------------------------------------------------------------
%% @doc
%% Find the last element of a list.
%% @end
%%-------------------------------------------------------------------------
last([]) ->
    {error, empty_list};
last([Last]) ->
    Last;
last([_H| Tail]) ->
    last(Tail).

%%-------------------------------------------------------------------------
%% @doc
%% Find the last but one element of a list.
%% @end
%%-------------------------------------------------------------------------
but_last([]) ->
    {error, empty_list};
but_last([ButLast, _Last]) ->
    ButLast;
but_last([_H| Tail]) ->
    but_last(Tail).

%%-------------------------------------------------------------------------
%% @doc
%% Find the K'th element of a list. The first element in the list is number
%% 1.
%% @end
%%-------------------------------------------------------------------------
element_at(Pos, _List)
  when Pos =< 0 ->
    {error, wrong_position};
element_at(_Pos, []) ->
    {error, wrong_position};
element_at(1, [H| _Tail]) ->
    H;
element_at(Pos, [_H| Tail]) ->
    element_at(Pos - 1, Tail).

%%-------------------------------------------------------------------------
%% @doc
%% Find the number of elements of a list.
%% @end
%%-------------------------------------------------------------------------
length(List) ->
    '_length'(List, _Length = 0).

'_length'([], Length) ->
    Length;
'_length'([_H| Tail], Length) ->
    '_length'(Tail, Length + 1).

%%-------------------------------------------------------------------------
%% @doc
%% Reverse a list.
%% @end
%%-------------------------------------------------------------------------
reverse(List) ->
    '_reverse'(List, _ReversedList = []).

'_reverse'([], ReversedList) ->
    ReversedList;
'_reverse'([H| Tail], ReversedList) ->
    '_reverse'(Tail, [H| ReversedList]).

%%-------------------------------------------------------------------------
%% @doc
%% Find out whether a list is a palindrome. A palindrome can be read
%% forward or backward; e.g. "xamax".
%% @end
%%-------------------------------------------------------------------------
is_palindrome(List) ->
    '_is_palindrome'(List, reverse(List)).

%% This is a naive implementation.
'_is_palindrome'([] = _List1, [] = _List2) ->
    true;
'_is_palindrome'([] = _List1, _List2) ->
    false;
'_is_palindrome'(_List1, [] = _List2) ->
    false;
'_is_palindrome'([H1| Tail1], [H2| Tail2])
  when H1 == H2 ->
    '_is_palindrome'(Tail1, Tail2);
'_is_palindrome'(_List1, _List2) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% Flatten a nested list structure.
%%
%% Transform a list, possibly holding lists as elements into a 'flat' list
%% by replacing each list with its elements (recursively).
%% @end
%%-------------------------------------------------------------------------
flatten(List) ->
    '_flatten'(List, []).

'_flatten'([], FlattenedList) ->
    FlattenedList;
'_flatten'([H| Tail], FlattenedList)
  when is_list(H) ->
    '_flatten'(Tail, FlattenedList ++ flatten(H));
'_flatten'([H| Tail], FlattenedList) ->
    '_flatten'(Tail, FlattenedList ++ [H]).

%%-------------------------------------------------------------------------
%% @doc
%% Eliminate consecutive duplicates of list elements.
%%
%% If a list contains repeated elements they should be replaced with a
%% single copy of the element. The order of the elements should not be
%% changed.
%% @end
%%-------------------------------------------------------------------------
compress([]) ->
    [];
compress([H| Tail]) ->
    '_compress'(Tail, {H, [H]}).

'_compress'([], {_Prev, CompressedList} = _Acc) ->
    reverse(CompressedList);
'_compress'([H| Tail], {Prev, CompressedList} = Acc)
  when H == Prev ->
    '_compress'(Tail, Acc);
'_compress'([H| Tail], {Prev, CompressedList} = _Acc) ->
    '_compress'(Tail, {H, [H| CompressedList]}).

%%-------------------------------------------------------------------------
%% @doc
%% Pack consecutive duplicates of list elements into sublists. If a list
%% contains repeated elements they should be placed in separate sublists.
%% @end
%%-------------------------------------------------------------------------
pack(_List) ->
    oops.

%%-------------------------------------------------------------------------
%% @doc
%% Run-length encoding of a list.
%%
%% Consecutive duplicates of elements are encoded as lists (N E) where N is
%% the number of duplicates of the element E.
%% @end
%%-------------------------------------------------------------------------
encode(_List) ->
    oops.


%% =========================================================================
%%  Unit tests
%% =========================================================================

-ifdef(TEST).

last_test() ->
    5 = last([1,2,3,4,5]).

but_last_test() ->
    4 = but_last([1,2,3,4,5]).

element_at_test() ->
    3 = element_at(3, [1,2,3,4,5]).

length_test() ->
    5 = length([1,2,3,4,5]).

reverse_test() ->
    [5,4,3,2,1] = reverse([1,2,3,4,5]).

is_palindrome_test() ->
    true = is_palindrome("madamimadam").

flatten_test() ->
    [1,2,3,4,5] = flatten([1,[[2,3],4],5]).

compress_test() ->
    [$A,$B,$C,$A,$D,$E] =
        compress("AAAABCCAADEEEE").

pack_test() ->
    [[$A,$A,$A,$A],[$B],[$C,$C],[$A,$A],[$D],[$E,$E,$E,$E]] =
        pack("AAAABCCAADEEEE").

encode_test() ->
    [{4,$A},{1,$B},{2,$C},{2,$A},{1,$D},{4,$E}] =
        encode("AAAABCCAADEEEE").

-endif.
