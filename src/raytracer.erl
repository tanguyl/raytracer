-module(raytracer).

-export([p3/0]).


%Ray-related functions.
-record(ray, {orig, dest}).

ray_at(#ray{orig=O, dest=D}, t) ->
    numerl:plus(O, numerl:mult(D,t)).


%Render-related functions.

write_color(S, Vector)->
    io:format(S, "~B ~B ~B~n", numerl:mtfli(Vector)).

p3()->
    Image_width = 255,
    Image_height = 255,

    Factors = numerl:matrix([[255.99/(Image_width-1), 255.99/(Image_height-1), 0.25*255.99]]),

    GenPixel = fun(I,J)->
                    Color = numerl:mult(numerl:matrix([[I,J,1]]), Factors),
                    if I == 0   -> [J, Color];
                    true        -> Color end
                end,
    Picture = [[ GenPixel(I,J) || I <- lists:seq(0,Image_width-1)] || J <- lists:seq(Image_height-1,0, -1)],
    
    {ok, S} = file:open("image.ppm", [write, {delayed_write, 700000, 40}]),
    WriteLines = 
        fun F([])->ok;
         F([Colors|T])->
                if is_tuple(Colors)->
                    write_color(S, Colors);
                true ->
                    io:format("\r~B remaining.", [Colors])
                end,
             F(T)
        end,

    io:format(S, "P3\n~B ~B\n255\n", [Image_width, Image_height]),
    WriteLines(lists:flatten(Picture)),
    file:close(S).
    

    
