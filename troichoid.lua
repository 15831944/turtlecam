function cycloid()

r = 10
for t = 1, 10, 0.1 do

    x = r * (t - math.sin(t))
    y = r * (1 - math.cos(t))
    move_to(x, y, nil)

end

end

function half_circle(r, f, sides)
    sides = sides or 64
    p = 1 * math.pi * r
    for i = 0, sides do
        cut(p/sides, f)
        turn(-360/sides/2)
    end
    turn(360/sides/2)
end

-- arc
-- short line
-- arc
-- longer line
-- repeat
function trochoidal_slot(r, dist, step, f)
    steps = dist / step
    for _ = 1, steps do
        half_circle(r, f)
        cut(step, f)
        half_circle(r, f)
        cut(step*2, f)
    end
end

step=1
r=10/2
f=200

for i = 1, 4 do
    trochoidal_slot(r, 50, step, f)
    turn(90)
end

