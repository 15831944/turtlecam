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

function involute()
    x = r * (math.cos(t) + (t * math.sin(t)))
    y = r * (math.sin(t) - (t * math.cos(t)))
end

tool_r = 4
f = 200
r = 20 - tool_r
z = 7

--turn(90)
--move_to(10,10,nil)
helical_plunge(tool_r, 7, 0.5, f)
cam.spiral(pos(), r, 0.5, f)
arc_to(pos().x, pos().y, nil, 1, -r, 0, 1, f)
