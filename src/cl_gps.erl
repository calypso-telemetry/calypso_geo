-module(cl_gps).
-author("Sergey Loguntsov").

-include("gps.hrl").

%% API
-export([
  new/4, new/1, new/2,
  is_gps/1,
  x/1,y/1, z/1, precision/1,
  distance/2, azimuth/2
]).

new(X,Y,Z, Precision) ->
  #gps{
    x = X,
    y = Y,
    z = Z,
    precision = Precision
    %geohash = gps_geohash:encode(X,Y, Precision)
  }.

is_gps(Gps) ->
  is_record(Gps, gps).

new({X,Y}) ->
  new(X,Y, undefined, undefined);

new({X,Y,Z}) ->
  new(X, Y, Z, undefined).

new({X,Y}, Precision) ->
  new(X, Y, undefined, Precision);

new({X,Y,Z}, Precision) ->
  new(X, Y, Z, Precision).

x(undefined) -> undefined;
x(Gps) -> Gps#gps.x.

y(undefined) -> undefined;
y(Gps) -> Gps#gps.y.

z(undefined) -> undefined;
z(Gps) -> Gps#gps.z.

precision(undefined) -> undefined;
precision(Gps) -> Gps#gps.precision.

distance(undefined, _ ) -> undefined;
distance(_, undefined) -> undefined;
distance(Gps1, Gps2) when ?IS_GPS(Gps1), ?IS_GPS(Gps2) ->
  { X1, Y1 } = to_radian(Gps1),
  { X2, Y2 } = to_radian(Gps2),
  distance(X1, Y1, X2, Y2).

distance(Lon1, Lat1, Lon2, Lat2) ->
  %% Формула гаверсинусов
  6372795 * 2 * math:asin(math:sqrt(pow2((Lat1-Lat2)/2)+math:cos(Lon1)*math:cos(Lon2)*pow2(math:sin((Lon1-Lon2)/2)))).

azimuth(undefined, _) -> undefined;
azimuth(_, undefined) -> undefined;
azimuth(Gps1, Gps2) ->
  math:atan2(x(Gps1) - x(Gps2), y(Gps1) - y(Gps2)).

to_radian(Gps) ->
  {Gps#gps.x / 180 * math:pi(), Gps#gps.y * 180 / math:pi() }.

pow2(X) ->
  X * X.

