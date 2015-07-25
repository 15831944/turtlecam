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

function cam.plunge(dist, f)
    pitch(90)
    cut(dist, f)
    pitch(-90)
end

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

return cam
