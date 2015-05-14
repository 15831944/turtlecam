local cam = {}

function cam.plunge(dist, f)
    pitch(90)
    cut(dist, f)
    pitch(-90)
end

-- @zig - short side step
-- @zag - long side step
-- @num - number of steps
-- @turn_left - whether to turn left or right
-- @f - feedrate
function cam.square_zag(zig, zag, num, turn_left, f)
    local step, dir
    for step = 1, num do
        cut(zag, f)
        if step % 2 ~= 0 then
            dir = 1
        else
            dir = -1
        end
        if turn_left then
            dir = -dir
        end
        if step < num then
			turn(90*dir)
			cut(zig, f)
			turn(90*dir)
		end
    end
end

function cam.rectangle(a, b, turn_left, f)
    if turn_left then
        dir = -1
    else
        dir = 1
    end
    cut(a, f)
    turn(90*dir)
    cut(b, f)
    turn(90*dir)
    cut(a, f)
    turn(90*dir)
    cut(b, f)
    turn(90*dir)
end

function cam.square(size, f)
    for i = 1, 4 do
        cut(size, f)
        turn(90)
    end
end

function cam.circle(r, f, sides)
    sides = sides or 64
    p = 2 * math.pi * r
    for i = 1, sides do
        cut(p/sides, f)
        turn(360/sides)
    end
end

function deg2rad(deg)
    return (deg/1)*(math.pi/180)
end

function polygon_radius_to_edge(r, n)
    return 2 * r * math.sin(deg2rad(180/n))
end
function polygon_apothem(r, n)
    return r * math.cos(deg2rad(180/n))
end

-- @r - radius
-- @n - polygon sides
-- @draw_n - sides to cut
-- @center - {x=x, y=y}
-- @f - feedrate
function cam.polygon(r, n, draw_n, center, f)
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
        cut(edge, f)
        turn(theta)
    end
end

return cam
