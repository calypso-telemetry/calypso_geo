-module(cl_gps_geohash).
-author("Sergey Loguntsov").

-include("gps.hrl").



%% API
-export([
  encode/1, encode/3, decode/1,
  intersection/1, intersection/2,
  contains/2,
  is_less/2
]).

encode(Gps) when ?IS_GPS(Gps) ->
  encode(Gps#gps.x, Gps#gps.y, Gps#gps.precision).
encode(X,Y, undefined) ->
  encode(X,Y, 1);
encode(X,Y, Precision) when is_integer(X), is_integer(Y), is_integer(Precision) ->
  encode(vert, X,Y, Precision, { -180 * ?GPS_SCALE, 180 * ?GPS_SCALE }, { -90 * ?GPS_SCALE, 90 * ?GPS_SCALE }, { <<"">> , <<>>}).

encode(Type, X, Y, Precision, Xd, Yd, { Bytes, Acc }) when bit_size(Acc) =:= 8 ->
  encode(Type, X, Y, Precision, Xd, Yd, { << Bytes/bits, Acc/bits >>, <<>> });
encode(vert, X, Y, Precision, { Xmin, Xmax }, Y0, { Bytes, Acc }) ->
  Pd  = ( Xmax - Xmin ) div 2,
  if
    Pd =< Precision ->
      Bytes;
    Pd =< 1 ->
      Bytes;
    true ->
      Xd =  Xmin + Pd,
      if
        X < Xd ->
          encode(horiz, X, Y, Precision, { Xmin, Xd }, Y0, { Bytes, << Acc/bits, 0:1>>});
        true ->
          encode(horiz, X, Y, Precision, { Xd, Xmax }, Y0, { Bytes, <<Acc/bits, 1:1>>})
      end
  end;

encode(horiz, X, Y, Precision, X0, { Ymin, Ymax }, { Bytes, Acc }) ->
  Pd  = ( Ymax - Ymin ) div 2,
  if
    Pd =< Precision ->
      Bytes;
    Pd =< 1 ->
      Bytes;
    true ->
      Yd =  Ymin + Pd,
      if
        Y < Yd ->
          encode(vert, X, Y, Precision, X0, { Ymin, Yd }, { Bytes, << Acc/bits, 0:1>>});
        true ->
          encode(vert, X, Y, Precision, X0, { Yd, Ymax }, { Bytes, <<Acc/bits, 1:1>>})
      end
  end.

decode(Geohash) ->
  decode(vert, Geohash, { -180 * ?GPS_SCALE, 180 * ?GPS_SCALE }, { -90 * ?GPS_SCALE, 90 * ?GPS_SCALE }).

decode(_, <<>>, { Xmin, Xmax }, { Ymin, Ymax }) ->
  Px = (Xmax - Xmin) div 2,
  Py = (Ymax - Ymin) div 2,
  X = Xmin + Px,
  Y = Ymin + Py,
  Precision = max(Px, Py),
  { X,Y, Precision };

decode(vert, << 0:1, G/bits >>, { Xmin, Xmax }, Y) ->
  Pd = (Xmax - Xmin) div 2,
  decode(horiz, G, { Xmin, Xmin + Pd }, Y);

decode(vert, << 1:1, G/bits >>, { Xmin, Xmax }, Y) ->
  Pd = (Xmax - Xmin) div 2,
  decode(horiz, G, { Xmin + Pd, Xmax }, Y);

decode(horiz, << 0:1, G/bits >>, X, {Ymin, Ymax }) ->
  Pd = (Ymax - Ymin) div 2,
  decode(vert, G, X, { Ymin, Ymin + Pd });

decode(horiz, << 1:1, G/bits >>, X, {Ymin, Ymax }) ->
  Pd = (Ymax - Ymin) div 2,
  decode(vert, G, X, { Ymin + Pd, Ymax }).

intersection(Geohash1, Geohash2) ->
  intersection(Geohash1, Geohash2, <<>>).

intersection(<< X:1, G1/binary>>, << X:1, G2/binary >>, Acc) ->
  intersection(G1, G2, << Acc/bits, X:1 >>);
intersection(G1, G2, Acc) -> { Acc, G1 , G2 }.


intersection([]) -> undefined;
intersection([GeoHash]) -> GeoHash;
intersection([ Hash1, Hash2 | GeoHashList]) when is_list(GeoHashList) ->
  {SamePart, _, _ } = intersection(Hash1, Hash2),
  intersection([ SamePart | GeoHashList]).

contains(G, G) -> equal;
contains(G1, G2) ->
  contains(G1, G2, <<>>).

contains(<< X:1/bytes, Hash1/binary >>, << X:1/bytes, Hash2/binary >>, Acc) ->
  contains(Hash1, Hash2, <<Acc/binary, X:1/bytes>>);
contains(<< X:1/bits, Hash1/binary >>, << X:1/bits, Hash2/binary>>, Acc) ->
  contains(Hash1, Hash2, <<Acc/binary, X:1/bits>>);
contains(<<>>, Hash, Acc) -> { right, Acc, Hash };
contains(Hash, <<>>, Acc) -> { left, Acc, Hash };
contains(H1, H2, Acc) -> { none, Acc, H1, H2 }.

is_less(Hash1, Hash2) ->
  case contains(Hash1, Hash2) of
    { left, _, _ } -> true;
    _ -> false
  end.

