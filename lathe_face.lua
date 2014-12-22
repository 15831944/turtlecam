-- X0Z0 at center top of part
mode("lathe");

diameter = 25.4;
depth = 0.5;
step_size = 0.1;
radius = diameter/2;

steps = depth / step_size;

move_to(radius + .1, .1);
move_to(radius, 0);
print();
for i = 0, depth, step_size do
    turn(-90);
    cut(step_size, 50);
    turn(-90);
    cut(diameter/2, 50);
    turn(-90);
    move(.5);
    turn(-90);
    move(diameter/2);
    print();
end
