-module(raytracer).

-export([p3_numerl/0]).


write_color(S, Vector)->
    C = numerl:dot(Vector, 255),
    io:format(S, "~B ~B ~B~n", [round(numerl:at(C,1)), round(numerl:at(C,2)), round(numerl:at(C,3))]).

p3_numerl()->
    Image_width = 255,
    Image_height = 255,

    GenPixel = fun(I,J)->[J, numerl:matrix([[I/(Image_width-1), J/(Image_height-1), 0.25]])] end,
    Picture = [[ GenPixel(I,J) || I <- lists:seq(0,Image_width)] || J <- lists:seq(Image_height-1,-1, -1)],


    
    {ok, S} = file:open("image.ppm", [write]),
    WriteLines = 
        fun F([])->ok;
         F([J, Colors|T])->
             io:format("\r~B lines remaining", [J]),
             write_color(S, Colors),
             F(T)
        end,

    io:format(S, "P3\n~B ~B\n255\n", [Image_width+1, Image_height+1]),
    WriteLines(lists:flatten(Picture)),
    file:close(S).
    

    
