local cam = require("cam")

--cam.square(50, 1, 50)

function flower()
    for i = 1,36 do
        turn(10)
        square(50)
    end
end


--flower()

function polygon_spiral (sides, size)
    angle = 360/sides
    for edge = 1, size, (sides/size) do
        move(edge)
        turn(angle)
    end
end

--sides = 6;
--cam.polygon(10, sides, sides, {x=0,y=0}, 50)
--polygon_spiral(sides, 10)

feed_rate = 50

--cam.polygon(10, 3, 3, {x=0,y=0}, 50)
--cam.polygon(10, 4, 4, {x=0,y=0}, 50)
--cam.polygon(10, 5, 5, {x=0,y=0}, 50)
--cam.polygon(10, 6, 6, {x=0,y=0}, 50)
--cam.polygon(10, 7, 7, {x=0,y=0}, 50)

if false then
    for i = 0, 5 do
        cam.plunge(0.5, 50)

        --polygon_spiral(4, 50)
        cam.polygon(10, 6, 6, nil, 50)
    end
end

function spiral(center, steps, loops, a, b, start_theta, f)
	step = 2 * math.pi / steps;
	max = 2 * math.pi * loops;

    pos = (a + b * (0 + start_theta));
    x = center.x + pos * math.cos(0);
    y = center.y + pos * math.sin(0);
    move_to(x, y, nil)
    turn_to(0)
	
	for angle = 0, max, step do
		pos = (a + b * (angle + start_theta));
		x = center.x + pos * math.cos(angle);
		y = center.y + pos * math.sin(angle);

		cut_to(x, y, nil, f);
    end
end

function half_circle(r, sides)
    sides = sides or 64
    p = 1 * math.pi * r
    for i = 0, sides do
        cut(p/sides, 50)
        turn(-360/sides/2)
    end
    turn(360/sides/2)
end

function bearing_block_profile()
    turn(90)
    cut(12, 50)
    half_circle(12);
    cut(12, 50)
    turn(-90)
    cut(24, 50)
    turn(180)
end

if false then
    for i = 0, 10 do
        bearing_block_profile()
        pitch(90)
        cut(1, 50)
        pitch(-90)
    end
end

function spiral2(center, radius, gap, f, steps)
    steps = steps or 64
    loops = radius/gap;
    b = radius/(loops * 2*math.pi);
    spiral(center, steps, loops, 0, b, 0, f);
end

if false then
    steps = 64;
    tool_r = 2; -- 4mm tool
    stepover = 0.5; --mm
    if stepover >= (tool_r*2) then
        error("Cannot have 100% stepover!");
    end
    final_radius = 11;   --22mm diameter circle
    radius = final_radius-tool_r; --mm
    loops = radius/((tool_r*2)-stepover);
    b = radius/(loops * 2*math.pi);

    spiral({x=0,y=0}, steps, loops, 0, b, 0);
    turn(-60)

    cam.circle(radius, 1, 50)
end


function circle_clearing(radius, center, gap, f)
    radius = radius - gap/2
    move_to(center.x, center.y, nil);
    cam.polygon(radius, 64, 64, center, f)
    spiral2(center, radius, gap, f) -- 10% stepover
end

for i = 0, 10 do
    cam.plunge(0.5, 50)
    circle_clearing(25/2, {x=0,y=0}, 8, 50)
end

