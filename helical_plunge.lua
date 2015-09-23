local cam = require("cam")

--print("G02 X0 Y0 I1 J1 Z-10 P10 F10")

function involute_spiral(center, dir, r, steps, loops, f)
	local step = 2 * math.pi / steps;
	local max = 2 * math.pi * loops;

	for theta = 0, max, step do
        t = theta * dir
        x = r * (math.cos(t) + (t * math.sin(t)))
        y = r * (math.sin(t) - (t * math.cos(t)))

		cut_to(x+center.x, y+center.y, nil, f);
    end
end


function square(size)
    for _ = 1, 4 do
        move(size)
        turn(90)
    end
end

--for i = 1, 10 do
--    square(1);
--    move(1);
--end
--move_to(0,0,nil)
--involute_spiral(pos(), -1, math.pi/10, 64, 10, 200)

tool_r = 4
f = 200
r = 20 - tool_r
z = 7


for i = 1, 4 do
    move(20)
    p = pos()
    cam.helical_plunge(1, 5, 0.5, f)
    turn(90)
    move_to(nil, nil, p.z)
end

--cam.spiral(pos(), r, 0.5, f)
--arc_to(pos().x, pos().y, nil, 1, -r, 0, 1, f)
