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
last(_List) ->
    oops.

%%-------------------------------------------------------------------------
%% @doc
%% Find the last but one element of a list.
%% @end
%%-------------------------------------------------------------------------
but_last(_List) ->
    oops.

%%-------------------------------------------------------------------------
%% @doc
%% Find the K'th element of a list. The first element in the list is number
%% 1.
%% @end
%%-------------------------------------------------------------------------
element_at(_Pos, _List) ->
    oops.

%%-------------------------------------------------------------------------
%% @doc
%% Find the number of elements of a list.
%% @end
%%-------------------------------------------------------------------------
length(_List) ->
    oops.

%%-------------------------------------------------------------------------
%% @doc
%% Reverse a list.
%% @end
%%-------------------------------------------------------------------------
reverse(_List) ->
    oops.

%%-------------------------------------------------------------------------
%% @doc
%% Find out whether a list is a palindrome. A palindrome can be read
%% forward or backward; e.g. "xamax".
%% @end
%%-------------------------------------------------------------------------
is_palindrome(_List) ->
    oops.

%%-------------------------------------------------------------------------
%% @doc
%% Flatten a nested list structure.
%%
%% Transform a list, possibly holding lists as elements into a 'flat' list
%% by replacing each list with its elements (recursively).
%% @end
%%-------------------------------------------------------------------------
flatten(_List) ->
    oops.

%%-------------------------------------------------------------------------
%% @doc
%% Eliminate consecutive duplicates of list elements.
%%
%% If a list contains repeated elements they should be replaced with a
%% single copy of the element. The order of the elements should not be
%% changed.
%% @end
%%-------------------------------------------------------------------------
compress(_List) ->
    oops.

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
