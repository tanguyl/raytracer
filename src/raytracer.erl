-module(raytracer).

-export([render/0]).


-record(sphere,{center,radius,color}).
-record(ray, {orig, dir}).
-record(hit, {dist, color}).

%Vector related function.
%----------------------------------------------------

color(M)->
    numerl:mult(M,255).
color(C0,C1,C2)->
    numerl:mult(numerl:matrix([[C0,C1,C2]]), 255).

vec3(C0,C1,C2)->
    numerl:matrix([[C0,C1,C2]]).

sqrd_len(V)->
    N = numerl:nrm2(V),
    N*N.

unit_vec(Vec)->
    Nrm = numerl:nrm2(Vec),
    numerl:divide(Vec, Nrm*Nrm).

%Ray-related functions.
%----------------------------------------------------
ray_at(#ray{orig=O, dir=D}, T) ->
    numerl:add(O, numerl:mult(D,T)).

ray_color(Ray=#ray{})->
    Scene = [
            %#sphere{center=vec3(0,0,-1),      radius=0.5,  color=color(0.5,0.5,0.5)},
             #sphere{center=vec3(0,0,-100),    radius=60  ,  color=color(0.8,0.8,0)},
             #sphere{center=vec3(0,-100.5,-1), radius=100,  color=color(0.3,0.5,0.2)}],
    
    Filter = fun 
            (Object,none)->
                Val = hit(Object,Ray),
                Val;
            (Object, Hit)->
                Val = hit(Object,Ray),
                if Val == none ->
                    Hit;
                true -> 
                    if Val#hit.dist < Hit#hit.dist -> Val; true -> Hit end
                end
            end,

    Hitpoint = lists:foldr(Filter, none, Scene),
    if Hitpoint#hit.dist > 0.0 ->
        Hitpoint#hit.color;
    true->
        T = (numerl:at(unit_vec(ray_at(Ray, -1)),2) + 1) / 2.0,
        color(numerl:add(numerl:mult(vec3(1,1,1),1-T), numerl:mult(vec3(0.6, 0.7, 1.0),T)))
    end.

%Sphere-related functions.
%----------------------------------------------------
hit(#sphere{center=Center, radius=Radius, color=Color}, #ray{orig=O, dir=D})->
    Oc = numerl:sub(O,Center),
    A = sqrd_len(D),
    Half_b = numerl:vec_dot(Oc, D),
    C = sqrd_len(Oc) - (Radius*Radius),
    Discri = Half_b*Half_b - A*C,
    if Discri>0 ->
        Root0 = (-Half_b - math:sqrt(Discri))/A,
        if Root0 > 0 ->
            #hit{dist=Root0, color=Color};
        true->
            Root1 =  (-Half_b + math:sqrt(Discri))/A,
            if Root1 > 0 ->
                #hit{dist=Root1, color=Color};
            true->
                none
            end
        end;
    true ->
        none
    end. 

%Render-related functions.
%----------------------------------------------------
write_color(S, Vector)->
    io:format(S, "~B ~B ~B~n", numerl:mtfli(Vector)).

render()->
    %Screen.
    Aspect_ratio = 16/9,
    Image_width = 500,
    Image_height = round(Image_width / Aspect_ratio),


    %Camera.
    Viewport_height = 2.0,
    Viewport_width = Aspect_ratio * Viewport_height,
    Focal_length = 1.0,

    Origin = vec3(0, 0, 0),
    Horizontal = vec3(Viewport_width, 0, 0),
    Vertical = vec3(0, Viewport_height, 0),
    Lower_left_corner = numerl:eval([Origin, sub, numerl:divide(Horizontal,2), sub, numerl:divide(Vertical,2), sub, vec3(0,0, Focal_length)]),

    GenPixel = fun(I,J)->
                    U = I/(Image_width-1),
                    V = J/(Image_height-1),
                    R = #ray{orig=Origin, dir=numerl:eval([Lower_left_corner, add, numerl:mult(Horizontal,U), add, numerl:mult(Vertical,V), sub, Origin])},
                    Color = ray_color(R),

                    if I == 0   -> 
                        io:format("\r~B remaining.", [J]),
                        Color;
                    true        -> Color end
                end,

    Picture = [[ GenPixel(I,J) || I <- lists:seq(0,Image_width-1)] || J <- lists:seq(Image_height-1,0, -1)],
    
    {ok, S} = file:open("image.ppm", [write, {delayed_write, 70000000, 100}]),
    WriteLines = 
        fun F([])->ok;
         F([Colors|T])->
             write_color(S,Colors),
             F(T)
        end,

    io:format(S, "P3\n~B ~B\n255\n", [Image_width, Image_height]),
    io:format("Writing file...~n"),
    WriteLines(lists:flatten(Picture)),
    file:close(S).