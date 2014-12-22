-- X0Z0 at center top of part
mode("lathe");

diameter = 25.4;
depth = 0.5;
step_size = 0.1;
radius = diameter/2;

steps = depth / step_size;

move_to(radius + .1, .5);
move_to(radius, 0.5);
print();
for i = 0, steps-1 do
    turn(90);
    move(-0.5);
    cut(-step_size, 50);
    turn(90);
    cut(radius, 50);
    turn(90);
    move(-.5);
    turn(90);
    move(radius);
    print();
end
