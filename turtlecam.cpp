/* 
 * Copyright (C) 2014  Nicholas Gill
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

extern "C" {
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}
#include <stdexcept>

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

turtle t;

int lua_move(lua_State *L) {
    int n = lua_gettop(L);
    if(n != 1 || !lua_isnumber(L, 1)) {
        lua_pushstring(L, "move(dist)");
        lua_error(L);
    }

    auto dist = lua_tonumber(L, 1);
    move(t, dist);

    return 0;
}
int lua_cut(lua_State *L) {
    int n = lua_gettop(L);
    if(n != 2 || !lua_isnumber(L, 1) || !lua_isnumber(L, 2)) {
        lua_pushstring(L, "cut(dist, f)");
        lua_error(L);
    }

    auto dist = lua_tonumber(L, 1);
    auto f = lua_tonumber(L, 2);
    cut(t, dist, f);

    return 0;
}
int lua_turn(lua_State *L) {
    int n = lua_gettop(L);
    if(n != 1 || !lua_isnumber(L, 1)) {
        lua_pushstring(L, "turn(degrees)");
        lua_error(L);
    }

    auto degrees = lua_tonumber(L, 1);
    turn(t, degrees);

    return 0;
}
int lua_pitch(lua_State *L) {
    int n = lua_gettop(L);
    if(n != 1 || !lua_isnumber(L, 1)) {
        lua_pushstring(L, "pitch(degrees)");
        lua_error(L);
    }

    auto degrees = lua_tonumber(L, 1);
    pitch(t, degrees);

    return 0;
}
void turtle_open(lua_State* L) {
    lua_register(L, "move", lua_move);
    lua_register(L, "cut", lua_cut);
    lua_register(L, "turn", lua_turn);
    lua_register(L, "pitch", lua_pitch);
}


int main(int argc, char* argv[]) {
    lua_State* L = lua_open();
    luaL_openlibs(L);
    turtle_open(L);

    lua_newtable(L);
    for (int i = 1; i < argc; ++i) {
        lua_pushstring(L, argv[i]);
        lua_rawseti(L, -2, i);
    }
    lua_setglobal(L, "argv");

    if(argc >= 2) {
        int s = luaL_dofile(L, argv[1]);
        if(s) {
            std::string error;
            error = std::string("LUA: ") + lua_tostring(L, -1);
            std::cerr << error << "\n";
            lua_pop(L, 1); // remove error message
        }
    } else {
        move(t, 100);
        pitch(t, 90);
        cut(t, 10, 10);
        pitch(t, -90);
        for(int i = 0; i < 6; ++i) {
            turn(t, 60);
            cut(t, 100, 50);
        }
    }

    lua_close(L);
    L = NULL;
}
