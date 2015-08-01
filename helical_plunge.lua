local cam = require("cam")

--print("G02 X0 Y0 I1 J1 Z-10 P10 F10")
-- Note! if tool r >= tool_t there will be a post left in the middle
function helical_plunge(r, depth, stepdown, f)
    turns = depth / stepdown
    turn_to(0)
    move(-r)
    -- TODO depth is an absolute value 
    -- but expectation is relative...
    arc_to(nil, nil, -depth, 1, r, 0, turns, f);
    move(r)
end

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

turn(90)
move(1)
move(-1)
turn(-90)
involute_spiral(pos(), -1, math.pi/10, 64, 10, 200)

tool_r = 4
f = 200
r = 20 - tool_r
z = 7

--helical_plunge(tool_r, 7, 0.5, f)
--cam.spiral(pos(), r, 0.5, f)
--arc_to(pos().x, pos().y, nil, 1, -r, 0, 1, f)
