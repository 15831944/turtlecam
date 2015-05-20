local cam = require("cam")

width = tonumber(argv[2] or 100)
height = tonumber(argv[3] or 100)
depth = tonumber(argv[4] or 1)

print("( face " .. width .. "x" .. height .. "x" .. depth .. " )")

tool_diameter = 8
stepover = .9 -- 90%
stepdown = .5

for _ = 1, 2 do
    move(height)
    turn(90)
    move(width)
    turn(90)
end

width = width - tool_diameter
height = height - tool_diameter

effective_tool_width = tool_diameter * stepover
width_cuts = math.ceil(width / effective_tool_width)

move_to(tool_diameter/2,tool_diameter/2,0)
cam.face(width, height, width_cuts, depth, stepdown, 50, 20)
