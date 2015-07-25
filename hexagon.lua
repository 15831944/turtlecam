local cam = require("cam")
-- print(argv[1]);
--print("M06 T03")
--move(100)
--pitch(90)
--cut(10,10)
--pitch(-90)
--for i = 0, 5 do
--    turn(60)
--    cut(100, 50)
--end


cam.polygon_helix({x=0,y=0,z=0}, 16/2, 6, 10, 1, 50);
