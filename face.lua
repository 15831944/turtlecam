
-- @zig - short side step
-- @zag - long side step
-- @num - number of steps
-- @turn_left - whether to turn left or right
-- @f - feedrate
function square_zag(zig, zag, num, turn_left, f)
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

function rectangle(a, b, turn_left, f)
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

function plunge(dist, f)
    pitch(90)
    cut(dist, f)
    pitch(-90)
end

function face(width, height, num, depth, stepdown, f, plunge_f)
    local zig = width / num
    local zag = height
    local depth_passes = math.ceil(depth / stepdown)
    local even_width = num % 2 == 0

    for z = 1, depth_passes do
        local even_layer = z % 2 == 0
        print("( layer " .. z .. " )")
        plunge(depth / depth_passes, plunge_f)
        square_zag(zig, zag, num+1, not even_width and even_layer, f)
        turn(180)
        rectangle(height, width, not even_width and not even_layer, f)
    end
end

width = tonumber(argv[2] or 100)
height = tonumber(argv[3] or 100)
depth = tonumber(argv[4] or 1)

print("(" .. width .. " " .. height .. " " .. depth .. ")")

tool_diameter = 8
stepover = .9 -- 90%
stepdown = .5

effective_tool_width = tool_diameter * stepover
width_cuts = math.ceil(width / effective_tool_width)

move_to(0,0,0)
face(width, height, width_cuts, depth, stepdown, 50, 20)
