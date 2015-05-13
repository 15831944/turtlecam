local cam = require("cam")

width = tonumber(argv[2] or 20)
length = tonumber(argv[3] or 100)
depth = tonumber(argv[4] or 1)

print("( slot " .. width .. "x" .. length .. "x" .. depth .. " )")

move_to(0,0,0)


tool_diameter = 8
stepdown = 1

local depth_passes = math.ceil(depth / stepdown)

if tool_diameter > width then
    error "Tool too wide for slot"
elseif tool_diameter == width then
    -- straight
    for z = 1, depth_passes do
        cam.plunge(depth / depth_passes, 50)
        cut(length, 50)
        turn(180)
    end
else
    -- cycloid
    a = 1
    r = tool_diameter/2
    for t = 0, 10, 1 do
        x = r * ((a*t) - math.sin(t))
        y = r * (a - math.cos(t))
        move_to(x, y, 0)
    end
end
