local cam = {}

function cam.plunge(dist, f)
    pitch(90)
    cut(dist, f)
    pitch(-90)
end

-- @zig - short side step
-- @zag - long side step
-- @num - number of steps
-- @turn_left - whether to turn left or right
-- @f - feedrate
function cam.square_zag(zig, zag, num, turn_left, f)
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

function cam.rectangle(a, b, turn_left, f)
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

return cam
