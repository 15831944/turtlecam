w = 100;
h = 100;
d = 2;
tool_d = 8;
stepdown = 0.1;
stepover = tool_d / 10;

for j = 0, (d/stepdown)-1 do
    pitch(90)
    cut(stepdown, 20)
    pitch(-90)
    if j % 2 then
        dir = 1
    else
        dir = -1
    end
    for i = 0, h/(tool_d-stepover)/2 do
        cut(w, 50)
        turn(90*dir)
        cut(tool_d - stepover, 50)
        turn(90*dir)
        cut(w, 50)
        turn(-90*dir)
        cut(tool_d - stepover, 50)
        turn(-90*dir)
    end
    turn(270)
end
