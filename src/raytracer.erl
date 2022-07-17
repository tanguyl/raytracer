-module(raytracer).

-export([render/0]).
-import(math, [sqrt/1]).
-import(rand, [uniform/0]).


-record(sphere,{center, radius, material}).
-record(ray, {origin, direction}).
-record(hit, {p, normal, t, front_face, material}).
-record(material, {name, aldebo}).
-record(camera, { origin, horizontal, vertical, lower_left_corner}).

%Vector/color/basic math related function.
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
reflect(V, N)->
    numerl:eval([V, sub, numerl:mult(numerl:mult(N, numerl:vec_dot(V,N)),2)]).

random_double()->
    1.0-rand:uniform().
random_double(Min, Max)->
    Min + (Max-Min)*random_double().

random_vec()->
    numerl:matrix([[random_double(), random_double(), random_double()]]).
random_vec(Min, Max)->
    numerl:matrix([[random_double(Min, Max), random_double(Min, Max), random_double(Min, Max)]]).
random_in_unit_sphere()->
    V = random_vec(-1,1),
    In_sphere = squared_length(V) < 1,
    if In_sphere ->
        V;
    true->
        random_in_unit_sphere()
    end.
random_unit_vector()->
    unit_vector(random_vec(-1,1)).
near_zero(V)->
    lists:all(fun(Val)->abs(Val) < 0.00000001 end, numerl:mtfl(V)).

clamp(Val, Min, Max)->
    if Val < Min -> Min;
    true ->
        if Val > Max -> Max;
        true -> Val
        end
    end.



%Hit related functions.
%--------------------------------------------------
%Create a new record whith the "front_face" variable set.
is_face_normal(R=#ray{}, Outward_normal)->
    numerl:vec_dot(R#ray.direction, Outward_normal) < 0.

%Material related functions.
%---------------------------------------------------

%Produce a scattered ray; and indicate how it should be attenuated.
scatter(Material=#material{}, Ray_in=#ray{}, Hit=#hit{})->
    case Material#material.name of 
    lambertian->
        Potential_scatter_direction = numerl:add(Hit#hit.normal, random_unit_vector()),
        Near_zero = near_zero(Potential_scatter_direction),
        if Near_zero ->
            %io:format("Near zero."),
            Scatter_direction = Hit#hit.normal;
        true ->
            %io:format("Not near zero."),
            Scatter_direction = Potential_scatter_direction
        end,
        {Material#material.aldebo, ray(Hit#hit.p, Scatter_direction)};
    
    metal->
        Reflected = reflect(unit_vector(Ray_in#ray.direction), Hit#hit.normal),
        Scattered = ray(Hit#hit.p, Reflected),
        VD = numerl:vec_dot(Scattered#ray.direction, Hit#hit.normal),
        if VD > 0 ->
            {Material#material.aldebo, Scattered};
        true->
            false
        end;
    _ ->
        false
    end.


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
                    #hit{t=Root, p=Pos, normal=Normal, front_face=is_face_normal(Ray, Normal), material=Object#sphere.material}
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
ray_color(_,_,0)->
    vec3(0,0,0);
ray_color(Ray=#ray{}, HittableList, Depth)->
    Hit = hit_in_list(HittableList, Ray, 0.001, 10000000),
    if
        Hit == false->
            %Background color
            Unit_direction = unit_vector(Ray#ray.direction),
            T = 0.5 * (numerl:at(Unit_direction, 2) + 1),
            numerl:add(numerl:mult(vec3(1.0, 1.0, 1.0), 1 - T), numerl:mult(vec3(0.5, 0.7, 1.0), T));
        true ->
            case scatter(Hit#hit.material, Ray, Hit) of
                {Attenuation, Scattered} ->
                    numerl:mult(ray_color(Scattered, HittableList, Depth - 1), Attenuation);
                _ ->
                    vec3(0,0,0)
            end
    end.
    


%Camera related functions.
%---------------------------------------------------
camera(Aspect_ratio)->
    Viewport_height = 2.0,
    Viewport_width = Aspect_ratio * Viewport_height,
    Focal_length = 1.0,

    Origin = vec3(0, 0, 0),
    Horizontal = vec3(Viewport_width, 0, 0),
    Vertical = vec3(0, Viewport_height, 0),
    Lower_left_corner = numerl:eval([Origin, sub, numerl:divide(Horizontal,2), sub, numerl:divide(Vertical,2), sub, vec3(0,0, Focal_length)]),

    #camera{origin=Origin, horizontal=Horizontal, vertical=Vertical, lower_left_corner=Lower_left_corner}.

    

camera_get_ray(Camera=#camera{}, U, V)->
   #ray{origin=Camera#camera.origin, direction=numerl:eval([Camera#camera.lower_left_corner,
                                                            add,numerl:mult(Camera#camera.horizontal, U),
                                                            add,numerl:mult(Camera#camera.vertical, V),
                                                            sub, Camera#camera.origin])}.

%IO related functions.
%---------------------------------------------------
avg_color(Color, Samples_per_pixel)->
    lists:map(fun (V)-> round(math:sqrt(V) * 255) end, numerl:mtfl(numerl:divide(Color, Samples_per_pixel))).


%Render-related functions.
%----------------------------------------------------
render()->
    %Screen.
    Aspect_ratio = 16/9,
    Image_width = 500,
    Image_height = round(Image_width / Aspect_ratio),
    Samples_per_pixel = 500,
    Max_depth = 40,

    %Scene.
    World = [
                #sphere{center=vec3(1.1,0,-1),radius = 0.5, material=#material{name=metal, aldebo=vec3(0.8, 0.8, 0.8)}},
                #sphere{center=vec3(-1.1,0,-1),radius = 0.5, material=#material{name=metal, aldebo=vec3(0.8, 0.6, 0.2)}},
                #sphere{center=vec3(0,0,-1),radius = 0.5, material=#material{name=lambertian, aldebo=vec3(0.8, 0.8, 0.0)}},
                #sphere{center=vec3(0,-100.5,-1),radius=100, material=#material{name=lambertian, aldebo=vec3(0.7, 0.3, 0.3)}}        
            ],

    Camera = camera(Aspect_ratio),

    GenPixel = 
        fun F(_,_, 0, Color)->
            Color;
        F(I,J, Sample, Color)->
            U = (I + random_double())/(Image_width-1),
            V = (J + random_double())/(Image_height-1),
            Ray = camera_get_ray(Camera, U, V),
            C = ray_color(Ray, World, Max_depth),
            %io:format("~f ~f ~f ~n", numerl:mtfl(C)),
            F(I, J, Sample-1, numerl:add(Color, C))
        end,

    Picture = [[ GenPixel(I,J, Samples_per_pixel, numerl:matrix([[0,0,0]])) || I <- lists:seq(1,Image_width-1)] || J <- lists:seq(Image_height-1,1, -1)],
    
    {ok, S} = file:open("renders/image.ppm", [write, {delayed_write, 70000000, 100}]),
    WriteLines = 
        fun F([])->ok;
         F([Colors|T])->
            io:format(S, "~B ~B ~B~n", avg_color(Colors, Samples_per_pixel)),
             F(T)
        end,

    io:format(S, "P3\n~B ~B\n255\n", [Image_width - 1, Image_height - 1]),
    io:format("Writing file...~n"),
    WriteLines(lists:flatten(Picture)),
    file:close(S).
