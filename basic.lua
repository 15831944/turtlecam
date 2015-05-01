-- print(argv[1]);
--move(100)
pitch(90)
cut(10,10)
pitch(-90)
for x = 0, 2 do
    if x % 2 == 0 then
        dir = -1
    else
        dir = 1
    end
    for y = 0, 10 do
        for side = 0, 5 do
            turn(60)
            cut(100, 50)
        end
        turn(90)
        move(200*dir)
        if dir < 0 then
            move(50)
        end
        turn(-90)
    end
    if dir < 0 then
        turn(180)
    end
    move(200)
    if dir < 0 then
        turn(-180)
    end
end

