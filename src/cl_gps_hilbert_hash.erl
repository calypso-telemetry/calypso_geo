-module(cl_gps_hilbert_hash).
-author("begemot").

%% API
-export([
  encode/3, decode/2
]).

%% По мотивам Генри Уорен мл. "Алгоритмические трюки для прогррамистов", Вильямс, 2003
%% стр 240

encode(N, X,Y) when is_integer(X), is_integer(Y), is_integer(N) ->
  encode(<<X:N>>,<<Y:N>>, <<>>, a).

encode(<<>>, <<>>, S, _) -> S;
encode(<<X0:1/bits, X/bits>>, <<Y0:1/bits, Y/bits>>, S, State) ->
  { NewS, NewState } = enc(State, X0, Y0),
  encode(X, Y, <<S/bits, NewS/bits>>, NewState).

enc(a, <<0:1>>,<<0:1>> ) -> { <<2#00:2>>, b };
enc(a, <<0:1>>,<<1:1>> ) -> { <<2#01:2>>, a };
enc(a, <<1:1>>,<<0:1>> ) -> { <<2#11:2>>, d };
enc(a, <<1:1>>,<<1:1>> ) -> { <<2#10:2>>, a };

enc(b, <<0:1>>,<<0:1>> ) -> { <<2#00:2>>, a };
enc(b, <<0:1>>,<<1:1>> ) -> { <<2#11:2>>, c };
enc(b, <<1:1>>,<<0:1>> ) -> { <<2#01:2>>, b };
enc(b, <<1:1>>,<<1:1>> ) -> { <<2#10:2>>, b };

enc(c, <<0:1>>,<<0:1>> ) -> { <<2#10:2>>, c };
enc(c, <<0:1>>,<<1:1>> ) -> { <<2#11:2>>, b };
enc(c, <<1:1>>,<<0:1>> ) -> { <<2#01:2>>, c };
enc(c, <<1:1>>,<<1:1>> ) -> { <<2#00:2>>, d };

enc(d, <<0:1>>,<<0:1>> ) -> { <<2#10:2>>, d };
enc(d, <<0:1>>,<<1:1>> ) -> { <<2#01:2>>, d };
enc(d, <<1:1>>,<<0:1>> ) -> { <<2#11:2>>, a };
enc(d, <<1:1>>,<<1:1>> ) -> { <<2#00:2>>, c }.

decode(N, S) when is_binary(S), is_integer(N) ->
  decode(S, <<0:N>>, <<0:N>>).

decode(<<>>, X, Y ) -> { X, Y };
decode(<<2#00:2, S/bits>>, X, Y) ->
  decode(S, <<Y/bits, 0:1>>, <<X/bits, 0:1>>);
decode(<<2#01:2, S/bits>>, X, Y) ->
  decode(S, <<X/bits, 0:1>>, <<Y/bits, 1:1>>);
decode(<<2#10:2, S/bits>>, X, Y) ->
  decode(S, <<X/bits, 1:1>>, <<Y/bits, 1:1>>);
decode(<<2#11:2, S/bits>>, X, Y) ->
  decode(S, <<(invert(Y))/bits, 1:1>>, <<(invert(X))/bits, 0:1>>).

invert(<<>>) -> <<>>;
invert(<<0:1/bits, Bits/bits>>) ->
  invert(<<1:1, (invert(Bits))/binary>>);
invert(<<1:1/bits, Bits/bits>>) ->
  invert(<<0:1, (invert(Bits))/binary>>).