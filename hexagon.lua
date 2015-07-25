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


offset = 2;
for y = 1, 80, 2*(polygon_apothem(8,6)+offset) do
    for x = 0, 10 do
        hex_x = x * (4+ polygon_radius_to_edge(8, 6) + offset);
        hex_y = y;
        if x % 2 == 0 then
            hex_y = hex_y + (polygon_apothem(8,6) + offset);
        end
        cam.polygon_helix({x=hex_x,y=hex_y,z=0}, 16/2, 6, 10, 1, 50);
        move_to(nil,nil,1);
    end
end
