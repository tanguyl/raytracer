
-module(raytracer_test).
-include_lib("eunit/include/eunit.hrl").

render_test()->
    ok = raytracer:render().