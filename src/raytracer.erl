-module(raytracer).

-export([render/0]).
-import(math, [sqrt/1]).


-record(sphere,{center,radius,color}).
-record(ray, {origin, direction}).
-record(hit, {p, normal, t, front_face}).

%Vector/color related function.
%----------------------------------------------------

color(M)->
    numerl:mult(M,255).
color(C0,C1,C2)->
    numerl:mult(numerl:matrix([[C0,C1,C2]]), 255).

vec3(C0,C1,C2)->
    numerl:matrix([[C0,C1,C2]]).

squared_length(V)->
    numerl:vec_dot(V,V).

unit_vector(V)->
    numerl:divide(V, numerl:nrm2(V)).

%Hit related functions.
%--------------------------------------------------
%Create a new record whith the "front_face" variable set.
is_face_normal(R=#ray{}, Outward_normal)->
    numerl:vec_dot(R#ray.direction, Outward_normal) < 0.


%Ray-related functions.
%---------------------------------------------------

ray(Origin, Direction)->
    #ray{origin=Origin, direction=unit_vector(Direction)}.

ray_at(Ray=#ray{}, T)->
    numerl:add(Ray#ray.origin, numerl:mult(Ray#ray.direction, T)).


hit(Object=#sphere{}, Ray, T_min, T_max)->
    Oc = numerl:sub(Ray#ray.origin, Object#sphere.center),
    A = squared_length(Ray#ray.direction),
    B = numerl:vec_dot(Oc, Ray#ray.direction),
    C = squared_length(Oc) - Object#sphere.radius * Object#sphere.radius,
    D = B*B - A*C,
    if 
        D < 0 ->
            false;
        true ->
            Sqrt_D = math:sqrt(D),
            Root = (-B - Sqrt_D)/A,
            if
                Root < T_min orelse T_max < Root ->
                    false;
                true ->
                    Pos = ray_at(Ray, Root),
                    Normal = numerl:divide(numerl:sub(Pos, Object#sphere.center), Object#sphere.radius),
                    #hit{t=Root, p=Pos, normal=Normal, front_face=is_face_normal(Ray, Normal)}
            end
    end.

hit_recursion([H|T], Ray, T_min, T_max, Best_Hit)->
    Hit = hit(H, Ray, T_min, T_max),
    if 
        Hit == false ->
            hit_recursion(T, Ray, T_min, T_max, Best_Hit);
        true ->
            hit_recursion(T, Ray, T_min, Hit#hit.t, Hit)
    end; 
hit_recursion(_,_,_,_,Best_Hit)->
    Best_Hit.

%Returns false if nothing was hit; else, returns the closest hit point.
hit_in_list(L, Ray, T_min, T_max) ->
    hit_recursion(L, Ray, T_min, T_max, false).

%World is a list of hittable object.
ray_color(Ray=#ray{}, HittableList)->
    Hit = hit_in_list(HittableList, Ray, 0, 100),
    if 
        Hit == false->
            %Background color
            Unit_direction = unit_vector(Ray#ray.direction),
            T = 0.5 * (numerl:at(Unit_direction, 2) + 1),
            numerl:add(numerl:mult(color(1.0, 1.0, 1.0), 1 - T), numerl:mult(color(0.5, 0.7, 1.0), T));
        true ->
            M = numerl:mult( numerl:add(Hit#hit.normal,1), 0.5),
            %io:format("~f ~f ~f ~n", [numerl:at(M,1), numerl:at(M,2), numerl:at(M,3)]),
            color(M)
    end.


%Render-related functions.
%----------------------------------------------------
render()->
    %Screen.
    Aspect_ratio = 16/9,
    Image_width = 600,
    Image_height = round(Image_width / Aspect_ratio),


    %Camera.
    Viewport_height = 2.0,
    Viewport_width = Aspect_ratio * Viewport_height,
    Focal_length = 1.0,

    Origin = vec3(0, 0, 0),
    Horizontal = vec3(Viewport_width, 0, 0),
    Vertical = vec3(0, Viewport_height, 0),
    Lower_left_corner = numerl:eval([Origin, sub, numerl:divide(Horizontal,2), sub, numerl:divide(Vertical,2), sub, vec3(0,0, Focal_length)]),

    %Scene.
    World = [#sphere{center=vec3(0,0,-1),radius = 0.5},
            #sphere{center=vec3(0,-100.5,-1),radius=100}],

    GenPixel = fun(I,J)->
                    U = I/(Image_width-1),
                    V = J/(Image_height-1),
                    R = ray(Origin, numerl:eval([Lower_left_corner, add, numerl:mult(Horizontal,U), add, numerl:mult(Vertical,V), sub, Origin])),
                    %io:format("~B ~B ~f~n", [I, J, sqrd_len(R#ray.direction)]),
                    Color = ray_color(R, World),
                    if I == 0   -> 
                        io:format("\r~B remaining.", [J]),
                        Color;
                    true        -> Color end
                end,

    Picture = [[ GenPixel(I,J) || I <- lists:seq(1,Image_width-1)] || J <- lists:seq(Image_height-1,1, -1)],
    
    {ok, S} = file:open("image.ppm", [write, {delayed_write, 70000000, 100}]),
    WriteLines = 
        fun F([])->ok;
         F([Colors|T])->
            io:format(S, "~B ~B ~B~n", numerl:mtfli(Colors)),
             F(T)
        end,

    io:format(S, "P3\n~B ~B\n255\n", [Image_width - 1, Image_height - 1]),
    io:format("Writing file...~n"),
    WriteLines(lists:flatten(Picture)),
    file:close(S).
