-module(geometry).
-export([area/1]).
area({rectangle, Width, Height}) -> Width * Height;
area({circle, Radius})           -> 3.14159265 * Radius * Radius;
area({square, Size})             -> Size * Size.
