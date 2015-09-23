
a = 100
b = 200
r = 8
f = 500

print("G02 X" .. -r .. " Y" ..r.. " J" ..r.. " F" ..f)
print("G01 X" .. -r .. " Y" .. a-r)
print("G02 X" .. 0 .. " Y" ..a.. " I" ..r)
print("G01 X" .. b-(2*r) .. " Y" .. a .. " F" ..f)
print("G02 X" .. b-r .. " Y" .. a-r .. " J" .. -r)
print("G01 X" .. b-r .. " Y" .. r)
print("G02 X" .. b-(2*r) .. " Y" .. 0 .. " I" .. -r)
print("G01 X" .. 0 .. " Y" .. 0)

--arc_to(-r, r, nil, 1, nil, r, 1, f)
--cut_to(-r, a-r, nil)
--arc_to(0, a, nil, 1, r, nil, 1, f)
--cut_to(a+(r/2), a, nil)
--arc_to(b-r, a-r, nil, 1, nil, -r, 1, f)
--cut_to(b-r, r, nil)
--arc_to(
