local cam = require("cam")
function cycloid()

r = 10
for t = 1, 10, 0.1 do

    x = r * (t - math.sin(t))
    y = r * (1 - math.cos(t))
    move_to(x, y, nil)

end

end

step=1
r=10/2
f=200

for i = 1, 4 do
    cam.trochoidal_slot(50, step, r, f)
    turn(90)
end

