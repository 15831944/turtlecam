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

auto r6(double n) -> std::string {
    std::ostringstream ss;
    ss << std::fixed << std::setprecision(6) << n;
    auto s = ss.str();

    s.erase(s.find_last_not_of('0') + 1, std::string::npos);
    if(s.back() == '.')
        s.pop_back();
    if(s == "-0") s = "0";
    return s;
};

inline bool is_equal(double a, double b, double tolerance = 1e-7) {
    return std::fabs(a - b) < tolerance;
}

struct vector {
    double x = 0.0;
    double y = 0.0;
    double z = 0.0;

    vector() = default;
    vector(double x, double y, double z)
     : x(x), y(y), z(z) {
    }

    vector operator+(const vector& a) const {
        return {x + a.x, y + a.y, z + a.z};
    }
    vector& operator+=(const vector& a) {
        x += a.x;
        y += a.y;
        z += a.z;
        return *this;
    }
};
vector operator*(const vector& v, double a) {
    return {v.x * a, v.y * a, v.z * a};
}


struct turtle {
    enum {
        mill,
        lathe
    } mode = mill;
    vector pos;
    /* Initial orientation is x:1,y:0,z:0 */
    double a = 0.0;
    double b = 90.0;
    enum {
        unknown,
        move,
        cut
    } motion = unknown;
    vector last_pos;
    double f = 0.0;

    vector orientation() const {
        static const double pi = 3.14159265359;
        auto theta = a * (pi/180);
        auto phi = b * (pi/180);
        return {std::cos(theta) * std::sin(phi), std::sin(theta) * std::sin(phi), std::cos(phi)};
    }
};
void move_to(turtle& t, double x, double y, double z) {
    t.pos = {x, y, z};
    std::cout << (t.motion == turtle::move ? "   " : "G00");

    if(t.mode == turtle::mill) {
        if(!is_equal(t.pos.x, t.last_pos.x))
            std::cout << " X" << r6(t.pos.x);
        if(!is_equal(t.pos.y, t.last_pos.y))
            std::cout << " Y" << r6(t.pos.y);
        if(!is_equal(t.pos.z, t.last_pos.z))
            std::cout << " Z" << r6(t.pos.z);
    } else {
        if(!is_equal(t.pos.x, t.last_pos.x))
            std::cout << " X" << r6(t.pos.x);
        if(!is_equal(t.pos.y, t.last_pos.y))
            std::cout << " Z" << r6(t.pos.y);
    }
    std::cout << "\n";

    t.motion = turtle::move;
    t.last_pos = t.pos;
    t.f = 0.0;
}
void move(turtle& t, double dist) {
    if(dist == 0.0) return;
    t.pos += t.orientation() * dist;
    std::cout << (t.motion == turtle::move ? "   " : "G00");
    if(t.mode == turtle::mill) {
        if(!is_equal(t.pos.x, t.last_pos.x))
            std::cout << " X" << r6(t.pos.x);
        if(!is_equal(t.pos.y, t.last_pos.y))
            std::cout << " Y" << r6(t.pos.y);
        if(!is_equal(t.pos.z, t.last_pos.z))
            std::cout << " Z" << r6(t.pos.z);
    } else {
        if(!is_equal(t.pos.x, t.last_pos.x))
            std::cout << " X" << r6(t.pos.x);
        if(!is_equal(t.pos.y, t.last_pos.y))
            std::cout << " Z" << r6(t.pos.y);
    }
    std::cout << "\n";

    t.motion = turtle::move;
    t.last_pos = t.pos;
    t.f = 0.0;
}
void cut(turtle& t, double dist, double f) {
    if(dist == 0.0) return;
    t.pos += t.orientation() * dist;
    std::cout << (t.motion == turtle::cut ? "   " : "G01");
    if(t.mode == turtle::mill) {
        if(!is_equal(t.pos.x, t.last_pos.x))
            std::cout << " X" << r6(t.pos.x);
        if(!is_equal(t.pos.y, t.last_pos.y))
            std::cout << " Y" << r6(t.pos.y);
        if(!is_equal(t.pos.z, t.last_pos.z))
            std::cout << " Z" << r6(t.pos.z);
    } else {
        if(!is_equal(t.pos.x, t.last_pos.x))
            std::cout << " X" << r6(t.pos.x);
        if(!is_equal(t.pos.y, t.last_pos.y))
            std::cout << " Z" << r6(t.pos.y);
    }
    if(!is_equal(f, t.f))
        std::cout << " F" << f;
    std::cout << "\n";

    t.motion = turtle::cut;
    t.last_pos = t.pos;
    t.f = f;
}
void turn(turtle& t, double degrees) {
    t.a += degrees;
}
void pitch(turtle& t, double degrees) {
    t.b += degrees;
}

turtle t;

int lua_mode(lua_State *L) {
    int n = lua_gettop(L);
    if(n != 1 || !lua_isstring(L, 1)) {
        lua_pushstring(L, "mode(mill|lathe)");
        lua_error(L);
        return 0;
    }

    std::string mode = lua_tostring(L, 1);
    if(mode == "mill")
        t.mode = turtle::mill;
    else if(mode == "lathe")
        t.mode = turtle::lathe;
    else {
        lua_pushstring(L, "mode(mill|lathe)");
        lua_error(L);
    }

    return 0;
}
int lua_move_to(lua_State *L) {
    int n = lua_gettop(L);
    if(t.mode == turtle::mill) {
        if(n != 3 || !lua_isnumber(L, 1) || !lua_isnumber(L, 2) || !lua_isnumber(L, 3)) {
            lua_pushstring(L, "move_to(x, y, z)");
            lua_error(L);
            return 0;
        }

        auto x = lua_tonumber(L, 1);
        auto y = lua_tonumber(L, 2);
        auto z = lua_tonumber(L, 3);
        move_to(t, x, y, z);
    } else {
        if(n != 2 || !lua_isnumber(L, 1) || !lua_isnumber(L, 2)) {
            lua_pushstring(L, "move_to(x, z)");
            lua_error(L);
            return 0;
        }

        auto x = lua_tonumber(L, 1);
        auto y = lua_tonumber(L, 2);
        auto z = 0;
        move_to(t, x, y, z);
    }

    return 0;
}
int lua_move(lua_State *L) {
    int n = lua_gettop(L);
    if(n != 1 || !lua_isnumber(L, 1)) {
        lua_pushstring(L, "move(dist)");
        lua_error(L);
        return 0;
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
        return 0;
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
        return 0;
    }

    auto degrees = lua_tonumber(L, 1);
    turn(t, degrees);

    return 0;
}
int lua_pitch(lua_State *L) {
    int n = lua_gettop(L);
    if(t.mode == turtle::mill) {
        if(n != 1 || !lua_isnumber(L, 1)) {
            lua_pushstring(L, "pitch(degrees)");
            lua_error(L);
            return 0;
        }

        auto degrees = lua_tonumber(L, 1);
        pitch(t, degrees);
    }

    return 0;
}
void turtle_open(lua_State* L) {
    lua_register(L, "mode", lua_mode);
    lua_register(L, "move_to", lua_move_to);
    lua_register(L, "move", lua_move);
    lua_register(L, "cut", lua_cut);
    lua_register(L, "turn", lua_turn);
    lua_register(L, "pitch", lua_pitch);
}


int main(int argc, char* argv[]) {
    if(argc < 2) {
        std::cerr << argv[0] << " command\n";
        return 1;
    }

    lua_State* L = luaL_newstate();
    luaL_openlibs(L);
    turtle_open(L);

    lua_newtable(L);
    for (int i = 1; i < argc; ++i) {
        lua_pushstring(L, argv[i]);
        lua_rawseti(L, -2, i);
    }
    lua_setglobal(L, "argv");

    if(luaL_dofile(L, argv[1])) {
        std::cerr << lua_tostring(L, -1) << "\n";
        lua_pop(L, 1);
    }

    lua_close(L);
    L = NULL;
}
