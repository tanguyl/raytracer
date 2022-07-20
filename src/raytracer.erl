-module(raytracer).

-export([render/0]).
-import(math, [sqrt/1]).
-import(rand, [uniform/0]).


-record(sphere,{center, radius, material}).
-record(ray, {origin, direction}).
-record(hit, {p, normal, t, front_face, material}).
-record(material, {name, aldebo, fuzz, ir}).
-record(camera, { origin, horizontal, vertical, lower_left_corner, lens_radius, u, v, w}).

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

cross(U,V)->
    [U0, U1, U2] = numerl:mtfl(U),
    [V0, V1, V2] = numerl:mtfl(V),
    numerl:matrix([[
        U1*V2 - U2*V1,
        U2*V0 - U0*V2,
        U0*V1 - U1*V0]]).

reflect(V, N)->
    numerl:eval([V, sub, numerl:mult(numerl:mult(N, numerl:vec_dot(V,N)),2)]).
refract(Uv, N, Etai_over_etat)->
    Cos_theta = lists:min([numerl:vec_dot(numerl:mult(Uv, -1),N), 1.0]),
    R_out_perp = numerl:mult(numerl:add(Uv, numerl:mult(N, Cos_theta)), Etai_over_etat),
    R_out_parallel = numerl:mult(N, -math:sqrt(abs(1 - squared_length(R_out_perp)))),
    numerl:add(R_out_perp, R_out_parallel).
reflectance(Cosine, Ref_idx)->
    R0 = math:pow((1.0-Ref_idx) / (1.0+Ref_idx), 2),
    R0 + (1-R0)*math:pow(1 - Cosine, 5).

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

random_in_unit_disk()->
    P = vec3(random_double(-1,1), random_double(-1,1), 0),
    L = squared_length(P),
    if 
        L >= 1  -> random_in_unit_disk();
        true    -> P
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

degrees_to_radians(Degrees)->
    Degrees*3.1415926535897932385/180.

%Hit related functions.
%--------------------------------------------------
%Create a new record whith the "front_face" variable set.
is_face_normal(R=#ray{}, Outward_normal)->
    numerl:vec_dot(R#ray.direction, Outward_normal) < 0.

%Material related functions.
%---------------------------------------------------
material_lambertian(Aldebo)->
    #material{name=lambertian, aldebo=Aldebo}.

material_metal(Aldebo, Fuzz)->
    #material{name=metal, aldebo = Aldebo, fuzz=clamp(Fuzz, 0,1)}.

material_dielectric(Index_of_refraction)->
    #material{name=dielectric, ir=Index_of_refraction}.

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
        Scattered = ray(Hit#hit.p, numerl:add(Reflected, numerl:mult(random_in_unit_sphere(), Material#material.fuzz))),
        VD = numerl:vec_dot(Scattered#ray.direction, Hit#hit.normal),
        if VD > 0 ->
            {Material#material.aldebo, Scattered};
        true->
            false
        end;

    dielectric->
        if Hit#hit.front_face ->
            Refraction_ratio = 1.0 / Material#material.ir;
        true->
            Refraction_ratio = Material#material.ir
        end,
        Unit_direction = unit_vector(Ray_in#ray.direction),
       
        Cos_theta = min(numerl:vec_dot(numerl:mult(Unit_direction, -1), Hit#hit.normal), 1.0),
        Sin_theta = sqrt(1 - Cos_theta*Cos_theta),
        Reflectance = reflectance(Cos_theta, Refraction_ratio) > random_double(),

        if Refraction_ratio * Sin_theta > 1 orelse Reflectance ->
            Direction = reflect(Unit_direction, Hit#hit.normal);
        true->
            Direction =  refract(Unit_direction, Hit#hit.normal, Refraction_ratio)
        end,
        {vec3(1.0, 1.0, 1.0), ray(Hit#hit.p, Direction)};
    _ ->
        false
    end.


%Ray-related functions.
%---------------------------------------------------

ray(Origin, Direction)->
    #ray{origin=Origin, direction=Direction}.

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
camera(Lookfrom, Lookat, Vup, Vfov, Aspect_ratio, Aperture, Focus_dist)->
    Theta = degrees_to_radians(Vfov),
    H = math:tan(Theta/2),
    Viewport_height = 2.0*H,
    Viewport_width = Aspect_ratio * Viewport_height,

    W = unit_vector(numerl:sub(Lookfrom, Lookat)),
    U = unit_vector(cross(Vup, W)),
    V = cross(W, U),


    Origin = Lookfrom,
    Horizontal = numerl:mult(numerl:mult(U,Viewport_width), Focus_dist),
    Vertical = numerl:mult(numerl:mult(V,Viewport_height), Focus_dist),
    Lower_left_corner = numerl:eval([Origin, sub, numerl:divide(Horizontal,2), sub, numerl:divide(Vertical,2), sub, numerl:mult(W, Focus_dist)]),

    #camera{origin=Origin, horizontal=Horizontal,
            vertical=Vertical, lower_left_corner=Lower_left_corner,
            lens_radius = Aperture/2,
            u=U, v=V, w=W}.

    

camera_get_ray(Camera=#camera{}, S, T)->
    Rd = numerl:mult(random_in_unit_disk(), Camera#camera.lens_radius),
    Offset = numerl:add(numerl:mult(Camera#camera.u, numerl:at(Rd,1)), numerl:mult(Camera#camera.v, numerl:at(Rd, 2))),
    #ray{   origin=numerl:add(Camera#camera.origin, Offset),
            direction=numerl:eval([Camera#camera.lower_left_corner,
                                    add,numerl:mult(Camera#camera.horizontal, S),
                                    add,numerl:mult(Camera#camera.vertical, T),
                                    sub, Camera#camera.origin,
                                    sub, Offset])}.

%IO related functions.
%---------------------------------------------------
avg_color(Color, Samples_per_pixel)->
    lists:map(fun (V)-> round(math:sqrt(V) * 255) end, numerl:mtfl(numerl:divide(Color, Samples_per_pixel))).


%Render-related functions.
%----------------------------------------------------

random_scene(World, 11, 11)->
    Cste_objects = [
        #sphere{center=vec3(0, -1000, 0), radius = 1000, material=material_lambertian(vec3(0.5, 0.5, 0.5))},
        #sphere{center=vec3(0,     1, 0), radius =    1, material=material_dielectric(3)},
        #sphere{center=vec3(-4,    1, 0), radius =    1, material=material_lambertian(vec3(0.4, 0.2, 0.1))},
        #sphere{center=vec3(4,     1, 0), radius =    1, material=material_metal(vec3(0.7, 0.6, 0.5), 0.05)} 
    ],
    lists:append(World, Cste_objects);
random_scene(World, Px, 11) ->
    random_scene(World, Px+1, -11);
random_scene(World, Px, Py) ->
    Center = vec3(Px+ 0.9*random_double(), 0.2, Py + 0.9*random_double()),
    Len = numerl:nrm2(numerl:sub(Center, vec3(4,0.2, 0))),
    if Len < 0.9 ->
        Next_world = World;
    true ->
        Pick_material = random_double(),
        if 
            Pick_material < 0.7 ->
                Aldebo = numerl:mult(random_vec(), random_vec()),
                Material = material_lambertian(Aldebo);
            Pick_material < 0.9 ->
                Aldebo = random_vec(0.1, 1),
                Fuzz = random_double(0, 0.4),
                Material = material_metal(Aldebo, Fuzz);
            true ->
                Material = material_dielectric(3)
        end,
        Obj = #sphere{center=Center, radius=0.2, material=Material},
        Next_world = [Obj|World] 
    end, 
    random_scene(Next_world, Px, Py+1).

random_scene()->
    random_scene([], -11, -11).

render()->
    Init_time = time(),

    %Screen.
    Aspect_ratio = 3/2,
    Image_width = 50,
    Image_height = round(Image_width / Aspect_ratio),
    Samples_per_pixel = 200,
    Max_depth = 50,

    %Scene.
    World = random_scene(),

    Lookfrom = vec3(13,2,3),
    Lookat   = vec3(0,0,0),
    Vup      = vec3 (0,1,0),
    Focus    = 10, 
    Aperture = 0.1,
    Camera   = camera(Lookfrom, Lookat, Vup, 20, Aspect_ratio, Aperture, Focus),

    GenPixel = 
        fun F(I, J, 0, Color)->
            io:format("\r~.2f%", [100*(I+(Image_height-J)*Image_width) /(Image_height*Image_width)]),
            Color;
        F(I,J, Sample, Color)->
            U = (I + random_double())/(Image_width-1),
            V = (J + random_double())/(Image_height-1),
            Ray = camera_get_ray(Camera, U, V),
            C = ray_color(Ray, World, Max_depth),
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
    Total_time = timer:now_diff(time(), Init_time),
    io:format("\rExecution time: ~b s~n", [Total_time]),
    io:format("~nWriting file...~n"),
    WriteLines(lists:flatten(Picture)),
    file:close(S).
