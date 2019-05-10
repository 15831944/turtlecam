function hilbert(d, f, depth, dir)
    if depth >= 1 then
        dir = dir or -1

        turn(-dir * 90)
        hilbert(d, f, depth-1, -dir)
        cut(d, f)
        turn(dir * 90)
        hilbert(d, f, depth-1, dir)
        cut(d, f)
        hilbert(d, f, depth-1, dir)
        turn(dir * 90)
        cut(d, f)
        hilbert(d, f, depth-1, -dir)
        turn(-dir * 90)
    end
end

feedrate = 50
tool_r = 4/2
depth = 6
hilbert(tool_r, feedrate, depth)
