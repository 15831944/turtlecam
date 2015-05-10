width = tonumber(argv[2] or 100)
height = tonumber(argv[3] or 100)
depth = tonumber(argv[4] or 1)

print("(" .. width .. " " .. height .. " " .. depth .. ")")

tool_diameter = 10
stepover = 1 -- .9 == 90%
stepdown = .3

effective_tool_width = tool_diameter * stepover

width_cuts = math.ceil(width / effective_tool_width)
depth_cuts = math.ceil(depth / stepdown)

move_to(0,0,0)

-- @zag - long side step
-- @zig - short side step
-- @num - number of steps
-- @turn_left - whether to turn left or right
-- @f - feedrate
function square_zag(zag, zig, num, turn_left, f)
    local step, dir
    for step = 0, num do
        cut(zag, f)
        if step % 2 == 0 then
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

for z = 0, depth_cuts - 1 do
	print("(layer " .. z+1 .. ")")
	pitch(90)
	cut(depth / depth_cuts, 20)
	pitch(-90)
    square_zag(height, width/width_cuts, width_cuts, width_cuts % 2 ~= 0 and z % 2 ~= 0, 50)
    turn(180)
end

turn(-180)

