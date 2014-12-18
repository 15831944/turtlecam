/* 
 * Copyright (C) 2013  Nicholas Gill
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <iostream>
#include <cmath>
#include <sstream>
#include <iomanip>
#include <string>

auto r6 = [](double n) -> std::string {
    std::ostringstream ss;
    ss << std::fixed << std::setprecision(6) << n;
    auto s = ss.str();
    
    s.erase(s.find_last_not_of('0') + 1, std::string::npos);
    if(s.back() == '.')
        s.pop_back();
    if(s == "-0") s = "0";
    return s;
};

struct vector {
    double x = 0.0;
    double y = 0.0;
    double z = 0.0;

    vector() = default;
    vector(double x, double y, double z)
     : x(x), y(y), z(z) {
    }
    
    vector operator+(vector a) const {
        return {x + a.x, y + a.y, z + a.z};
    }
    vector& operator+=(vector a) {
        x += a.x;
        y += a.y;
        z += a.z;
        return *this;
    }
};
vector operator*(vector v, double a) {
    return {v.x * a, v.y * a, v.z * a};
}


struct turtle {
    vector pos;
    /* Initial orientation is x:1,y:0,z:0 */
    double a = 0.0;
    double b = 90.0;

    vector orientation() const {
        static const double pi = 3.14159265359;
        auto theta = a * (pi/180);
        auto phi = b * (pi/180);
        return {std::cos(theta) * std::sin(phi), std::sin(theta) * std::sin(phi), std::cos(phi)};
    }
};
void move(turtle& t, double dist) {
    t.pos += t.orientation() * dist;
    std::cout << "G00 X" << r6(t.pos.x) << " Y" << r6(t.pos.y) << " Z" << r6(t.pos.z) << "\n"; 
}
void cut(turtle& t, double dist, double f) {
    t.pos += t.orientation() * dist;
    std::cout << "G01 X" << r6(t.pos.x) << " Y" << r6(t.pos.y) << " Z" << r6(t.pos.z) << " F" << f << "\n"; 
}
void turn(turtle& t, double degrees) {
    t.a += degrees;
}
void pitch(turtle& t, double degrees) {
    t.b += degrees;
}

int main() {
    turtle t;

    move(t, 100);
    pitch(t, 90);
    cut(t, 10, 10);
    pitch(t, -90);
    for(int i = 0; i < 6; ++i) {
        turn(t, 60);
        cut(t, 100, 50);
    }
}
