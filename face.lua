local cam = require("cam")

function face(width, height, num, depth, stepdown, f, plunge_f)
    local zig = width / num
    local zag = height
    local depth_passes = math.ceil(depth / stepdown)
    local even_width = num % 2 == 0

    for z = 1, depth_passes do
        local even_layer = z % 2 == 0
        print("( layer " .. z .. " )")
        cam.plunge(depth / depth_passes, plunge_f)
        cam.square_zag(zig, zag, num+1, not even_width and even_layer, f)
        turn(180)
        cam.rectangle(height, width, not even_width and not even_layer, f)
    end
end

width = tonumber(argv[2] or 100)
height = tonumber(argv[3] or 100)
depth = tonumber(argv[4] or 1)

print("( face " .. width .. "x" .. height .. "x" .. depth .. " )")

tool_diameter = 8
stepover = .9 -- 90%
stepdown = .5

effective_tool_width = tool_diameter * stepover
width_cuts = math.ceil(width / effective_tool_width)

move_to(0,0,0)
face(width, height, width_cuts, depth, stepdown, 50, 20)
