-module(raytracer).

-export([p3/0]).

%Vector related function.
%----------------------------------------------------

vec3(C0,C1,C2)->
    numerl:matrix([[C0,C1,C2]]).

unit_vec(Vec={matrix,_,_,_})->
    Nrm = numerl:dnrm2(Vec),
    numerl:divide(Vec, Nrm*Nrm).

%Ray-related functions.
%----------------------------------------------------
-record(ray, {orig, dir}).

ray_at(#ray{orig=O, dir=D}, t) ->
    numerl:plus(O, numerl:mult(D,t)).

ray_color(Ray=#ray{dir=D})->
    HitShpere = hit_sphere(vec3(0,0,-1), 0.5, Ray),
    if HitShpere ->
        vec3(255,0,0);
    true->
        T = (numerl:at(unit_vec(D),2) + 1) / 2.0,
        %T = (1+numerl:at(D,2))/2,
        R = numerl:mult(numerl:add(numerl:mult(vec3(1,1,1),1-T), numerl:mult(vec3(0.5, 0.7, 1.0),T)),255),
        R
    end.

%Sphere-related functions.
%----------------------------------------------------
hit_sphere(Center, Radius, #ray{orig=O, dir=D})->
    Oc = numerl:sub(O,Center),
    A = numerl:ddot(D,D),
    B = numerl:ddot(Oc,D)*2,
    C = numerl:ddot(Oc,Oc) - Radius * Radius,
    Discri = B*B - 4*A*C,
    Discri > 0. 

%Render-related functions.
%----------------------------------------------------
write_color(S, Vector)->
    io:format(S, "~B ~B ~B~n", numerl:mtfli(Vector)).

p3()->
    %Screen.
    Aspect_ratio = 16/9,
    Image_width = 400,
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