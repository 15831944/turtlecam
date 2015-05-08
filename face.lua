width = 100
height = 100
depth = 10

tool_diameter = 10
stepover = 1 -- .9 == 90%
stepdown = 1

effective_tool_width = tool_diameter * stepover

width_cuts = math.ceil(width / effective_tool_width)
depth_cuts = math.ceil(depth / stepdown)

move_to(0,0,0)

for z = 0, depth_cuts - 1 do
	print("(layer " .. z+1 .. ")")
	pitch(90)
	cut(depth / depth_cuts, 20)
	pitch(-90)

	for h = 0, width_cuts do
		cut(height, 50)
		if h % 2 == 0 then
			dir = 1
		else
			dir = -1
		end
		if h ~= width_cuts then
			turn(90*dir)
			cut(width / width_cuts, 50)
			turn(90*dir)
		end
	end
	turn(180)
end

turn(-180)
