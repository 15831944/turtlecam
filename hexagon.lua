local cam = require("cam")
-- print(argv[1]);
--print("M06 T03")
--move(100)
--pitch(90)
--cut(10,10)
--pitch(-90)
--for i = 0, 5 do
--    turn(60)
--    cut(100, 50)
--end

local function deg2rad(deg)
    return (deg/1)*(math.pi/180)
end
local function polygon_radius_to_edge(r, n)
    return 2 * r * math.sin(deg2rad(180/n))
end
local function polygon_apothem(r, n)
    return r * math.cos(deg2rad(180/n))
end


radius = 16/2;
offset = 2;
hexagons_x = 10;
hexagons_y = 10;
depth = 1;
f = 50;
turns = 10;

sides = 6;
apothem = polygon_apothem(radius, sides);
for y = 1, hexagons_y*(2*(apothem+offset)), 2*(apothem+offset) do
    for x = 1, hexagons_x do
        hex_x = x * ((radius/2) + polygon_radius_to_edge(radius, sides) + offset);
        hex_y = y;
        if x % 2 == 0 then
            hex_y = hex_y + (apothem + offset);
        end
        cam.polygon_helix({x=hex_x,y=hex_y,z=0}, radius, sides, turns, depth, f);
        move_to(nil,nil,1);
    end
end
