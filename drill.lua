
function peck_drill(depth, stepdown, f)
    p = pos();
    print("G83 X" .. p.x .. " Y" .. p.y .. " Z" .. p.z-depth .. " R" .. p.z .. " Q" .. stepdown .. " F" .. f)
end

peck_drill(5, 1, 50)
