local cam = {}

local function normalise_dir(dir)
    if dir < 0 then
        return -1
    else
        return 1
    end
end
local function even(num)
    return num % 2 == 0
end
local function odd(num)
    return num % 2 ~= 0
end
local function right_if(cond)
    if cond then
        return 1
    else
        return -1
    end
end
local function left_if(cond)
    if cond then
        return -1
    else
        return 1
    end
end


-- Simple vertical plunge
-- @depth - plunge depth
-- @f - feedrate
function cam.plunge(depth, f)
    pitch(90)
    cut(depth, f)
    pitch(-90)
end

-- Helical vertical plunge
-- @r - helix radius
-- @depth - plunge depth
-- @stepdown - depth per full circle
-- @f - feedrate
-- Note! if r >= tool_r there will be a post left in the middle
function cam.helical_plunge(r, depth, stepdown, f)
    turns = depth / stepdown
    p = pos();
    x = p.x+(r*math.cos(0))
    y = p.y+(r*math.sin(0))
    move_to(x, y, nil)
    arc_to(x, y, p.z-depth, 1, p.x-x, p.y-y, turns, f);
    move_to(p.x, p.y, nil)
end

-- Square zig-zag pattern
-- @zig - short side step
-- @zag - long side step
-- @num - number of steps
-- @dir - 1 = right, -1 = left
-- @f - feedrate
function cam.square_zag(zig, zag, num, dir, f)
    dir = normalise_dir(dir)
    for step = 1, num do
        cut(zag, f)
        local layer_dir = right_if(odd(step))
        if dir < 0 then
            layer_dir = -layer_dir
        end
        if step < num then
			turn(90*layer_dir)
			cut(zig, f)
			turn(90*layer_dir)
		end
    end
end

-- @a - first side
-- @b - second side
-- @dir - 1 = right, -1 = left
-- @f - feed rate
function cam.rectangle(a, b, dir, f)
    dir = normalise_dir(dir)
    for _ = 1, 2 do
        cut(a, f)
        turn(90*dir)
        cut(b, f)
        turn(90*dir)
    end
end

-- @size - side length
-- @dir - 1 = right, -1 = left
-- @f - feed rate
function cam.square(size, dir, f)
    dir = normalise_dir(dir)
    for _ = 1, 4 do
        cut(size, f)
        turn(90*dir)
    end
end

-- @r - radius
-- @dir - 1 = right, -1 = left
-- @f - feed rate
-- @sides - number of line segments
function cam.circle(r, dir, f, sides)
    dir = normalise_dir(dir)
    sides = sides or 64
    local p = 2 * math.pi * r
    for _ = 1, sides do
        cut(p/sides, f)
        turn(360/sides)
    end
end

local function deg2rad(deg)
    return (deg/1)*(math.pi/180)
end
local function polygon_radius_to_edge(r, n)
    return 2 * r * math.sin(deg2rad(180/n))
end
local function polygon_apothem(r, n)
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
        move(r)
        turn(90 + (theta/2));
    end

    for i = 1, draw_n do
        cut(edge, f)
        turn(theta)
    end

    if center then
        turn(-(90 + (theta/2)));
        turn(180)
        move(r)
        turn(180)
    end
end

function cam.face(width, height, num, depth, stepdown, f, plunge_f)
    local zig = width / num
    local zag = height
    local depth_passes = math.ceil(depth / stepdown)
    local even_width = num % 2 == 0

    for z = 1, depth_passes do
        local even_layer = even(z)
        print("( layer " .. z .. " )")
        cam.plunge(depth / depth_passes, plunge_f)
        cam.square_zag(zig, zag, num+1, left_if(not even_width and even_layer), f)
        turn(180)
        cam.rectangle(height, width, left_if(not even_width and not even_layer), f)
    end
end

function cam.polygon_helix(center, r, sides, turns, depth, f)
    move_to(center.x + r * math.cos(2 * math.pi * 0 / sides), center.y + r * math.sin(2*math.pi * 0 / sides), center.z);

    z = center.z;
    for _ = 1, turns do
        for side = 1, sides do
            x = center.x + r * math.cos(2 * math.pi * side / sides);
            y = center.y + r * math.sin(2 * math.pi * side / sides);
            if z then
                z = z - (depth / turns / sides);
            end
            cut_to(x, y, z, f);
        end
    end
end

function cam.tool(slot)
    print("M06 T" .. slot)
end

function cam.raw_spiral(center, steps, loops, a, b, start_theta, f)
	local step = 2 * math.pi / steps;
	local max = 2 * math.pi * loops;

    local pos = (a + b * (0 + start_theta));
    local x = center.x + pos * math.cos(0);
    local y = center.y + pos * math.sin(0);
    move_to(x, y, nil)
    turn_to(0)
	
	for angle = 0, max, step do
		pos = (a + b * (angle + start_theta));
		x = center.x + pos * math.cos(angle);
		y = center.y + pos * math.sin(angle);

		cut_to(x, y, nil, f);
    end
end

function cam.spiral(center, radius, gap, f, steps)
    steps = steps or 64
    loops = radius/gap;
    b = radius/(loops * 2*math.pi);
    cam.raw_spiral(center, steps, loops, 0, b, 0, f);
end

function cam.circle_clearing(radius, center, gap, f)
    radius = radius - gap/2
    move_to(center.x, center.y, nil);
    cam.polygon(radius, 64, 64, center, f)
    cam.spiral(center, radius-0.001, gap, f) -- 10% stepover
end

return cam
