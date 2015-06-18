local cam = require("cam")

function polygon_spiral (sides, size)
    angle = 360/sides
    for edge = 1, size-1, 0.5 do
        move(edge)
        turn(angle)
    end
end



function spiral(center, steps, loops, a, b, start_theta)
	step = 2 * math.pi / steps;
	max = 2 * math.pi * loops;

    pos = (a + b * (0 + start_theta));
    x = center.x + pos * math.cos(0);
    y = center.y + pos * math.sin(0);
    move_to(x, y, 0)
    turn_to(0)
	
	for angle = 0, max, step do
		pos = (a + b * (angle + start_theta));
		x = center.x + pos * math.cos(angle);
		y = center.y + pos * math.sin(angle);

		cut_to(x, y, 0, 50);
    end
end



function spiral2(center, radius, gap, steps)
    steps = steps or 64
    loops = radius/gap;
    b = radius/(loops * 2*math.pi);
    spiral(center, steps, loops, 0, b, 0);
end

function circle_clearing(r, step, f)
    turn(-90)
    move(step)
    turn(90)
    for i = step, r, step do
        cam.circle(i, 1, f)
        turn(-90)
        move(step)
        turn(90)
    end
end

--circle_clearing(20, 4, 50)

--cam.polygon(10, 4, 4, nil, 50)

cam.plunge(3, 50)

for j = 1, 5 do
    if j % 2 == 0 then
        dir = 1
        n = 19
    else
        dir = -1
        n = 20
    end
    if dir > 0 then
        move(10)
    end

    n = 20
    for i = 3, n do
        turn(30)
        cam.polygon(10, 6, 6, true, 50)
        turn(-30)
        if i ~= n then
            move(20)
        end
    end
    turn(90*dir)
    move(17)
    turn(90*dir)
    if dir > 0 then
        move(10)
    end
end
