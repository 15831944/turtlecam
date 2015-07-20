local cam = require("cam")

function square(size)
    for i = 1, 4 do
        move(size)
        turn(90)
    end
end

--square(50)

function flower()
    for i = 1,36 do
        turn(10)
        square(50)
    end
end


--flower()

function theta_spiral (size, angle)
    if size < 100 then
        move(size)
        turn(angle)
        theta_spiral (size + 2, angle)
    end
end

--theta_spiral(1, 60)
function polygon_spiral (sides, size)
    angle = 360/sides
    for edge = 1, size, 0.5 do
        move(edge)
        turn(angle)
    end
end

--polygon_spiral(6, 10)

feed_rate = 50

function deg2rad(deg)
    return (deg/1)*(math.pi/180)
end

function polygon_radius_to_edge(r, n)
    return 2 * r * math.sin(deg2rad(180/n))
end
function polygon_apothem(r, n)
    return r * math.cos(deg2rad(180/n))
end
function polygon(r, n, draw_n, center)
    draw_n = draw_n or n
    edge = polygon_radius_to_edge(r, n)
    theta = 360/n

    if center then
        x = center.x - r * math.cos(2 * math.pi / n)
        y = center.y - r * math.sin(2 * math.pi / n)
        move_to(x, y, 0)
        turn_to(0)
    end

    for i = 1, draw_n do
        cut(edge, feed_rate)
        turn(theta)
    end
end

--3 = 90
--4 = 45
--5 = 22.5

--polygon(10, 3, 3, {x=0,y=0})
polygon(10, 4, 4, {x=0,y=0})
--polygon(10, 5, 5, {x=0,y=0})
--polygon(10, 6, 6, {x=0,y=0})
--polygon(10, 7, 7, {x=0,y=0})

if false then
    for i = 0, 5 do
        pitch(90)
        cut(0.5, 50);
        pitch(-90)

        --polygon_spiral(4, 50)
        polygon(6, 50)
    end
end

function involute_spiral(phi)
    if phi < 1000 then
        turn(1)
        move (0.001 * phi)
        involute_spiral(phi + 1)
    end
end

function polyspi(theta, side, step)
    step = step or 1
    if side < 70 then
        turn (theta)
        move(side)
        polyspi (theta, side + step, step)
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

function circle(r, sides)
    sides = sides or 64
    p = 2 * math.pi * r
    for i = 1, sides do
        cut(p/sides, 50)
        turn(360/sides)
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

function spiral2(center, radius, gap, steps)
    steps = steps or 64
    loops = radius/gap;
    b = radius/(loops * 2*math.pi);
    spiral(center, steps, loops, 0, b, 0);
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

    circle(radius)
end

if false then
    sides = 6;
    spiral2({x=0,y=0}, polygon_apothem(10, sides), 1)
    polygon(9, sides, sides, {x=0,y=0})
    polygon(10, sides, sides, {x=0,y=0})
end

--spiral2({x=0,y=0}, 25/2, 4)
--circle(25/2)

function circle_clearing(radius, steps)
    turn(-90)
    move(1)
    turn(90)
    for i = 1, 20, 5 do
        circle(i)
        if (i+5) < 20 then
            turn(-90)
            move(5)
            turn(90)
        end
    end
end

