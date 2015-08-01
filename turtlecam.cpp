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
#include <boost/program_options.hpp>

extern "C" {
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

namespace po = boost::program_options;

void print_exception(const std::exception& e, int level = 0)
{
    std::cerr << std::string(level, ' ') << "error: " << e.what() << '\n';
    try
    {
        std::rethrow_if_nested(e);
    }
    catch(const std::exception& e)
    {
        print_exception(e, level+1);
    }
    catch(...)
    {
    }
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
        cut,
        arc_cw,
        arc_ccw
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
    if(is_equal(x, t.last_pos.x) && is_equal(y, t.last_pos.y) && is_equal(z, t.last_pos.z))
        return;
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
void cut_to(turtle& t, double x, double y, double z, double f) {
    if(is_equal(x, t.last_pos.x) && is_equal(y, t.last_pos.y) && is_equal(z, t.last_pos.z))
        return;
    t.pos = {x, y, z};
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
void arc_to(turtle& t, double x, double y, double z, double dir, double i, double j, double turns, double f) {
    t.pos = {x, y, z};
    if (dir >= 0)
        std::cout << (t.motion == turtle::arc_cw ? "   " : "G02");
    else
        std::cout << (t.motion == turtle::arc_ccw ? "   " : "G03");

    // XY non-optional for G17 arc.
    std::cout << " X" << r6(t.pos.x);
    std::cout << " Y" << r6(t.pos.y);
    if(!is_equal(t.pos.z, t.last_pos.z))
        std::cout << " Z" << r6(t.pos.z);

    std::cout << " I" << r6(i);
    std::cout << " J" << r6(j);
    if(!is_equal(turns, 1.0f))
        std::cout << " P" << r6(turns);

    if(!is_equal(f, t.f))
        std::cout << " F" << f;
    std::cout << "\n";

    t.motion = dir >= 0 ? turtle::arc_cw : turtle::arc_ccw;
    t.last_pos = t.pos;
    t.f = f;
}
/*void arc(turtle& t, double radius, double dir, double degrees, double z, double f) {
    // TODO
    double x = 0;
    double y = 0;
    double i;
    double j;

    if(is_equal(x, t.last_pos.x) && is_equal(y, t.last_pos.y) && is_equal(z, t.last_pos.z))
        return;
    t.pos = {x, y, z};
    if (dir >= 0)
        std::cout << (t.motion == turtle::arc_cw ? "   " : "G02");
    else
        std::cout << (t.motion == turtle::arc_ccw ? "   " : "G03");

    // XY non-optional for G17 arc.
    std::cout << " X" << r6(t.pos.x);
    std::cout << " Y" << r6(t.pos.y);
    if(!is_equal(t.pos.z, t.last_pos.z))
        std::cout << " Z" << r6(t.pos.z);

    std::cout << " I" << r6(i);
    std::cout << " J" << r6(j);
    if(!is_equal(turns, 1.0f))
        std::cout << " P" << r6(turns);

    if(!is_equal(f, t.f))
        std::cout << " F" << f;
    std::cout << "\n";

    t.motion = dir >= 0 ? turtle::arc_cw : turtle::arc_ccw;
    t.last_pos = t.pos;
    t.f = f;
}*/
void turn_to(turtle& t, double degrees) {
    t.a = degrees;
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
        if(n != 3 || ( !lua_isnumber(L, 1) && !lua_isnumber(L, 2) && !lua_isnumber(L, 3) ) ) {
            lua_pushstring(L, "move_to(x, y, z)");
            lua_error(L);
            return 0;
        }

        auto x = lua_isnil(L, 1) ? t.pos.x : lua_tonumber(L, 1);
        auto y = lua_isnil(L, 2) ? t.pos.y : lua_tonumber(L, 2);
        auto z = lua_isnil(L, 3) ? t.pos.z : lua_tonumber(L, 3);
        move_to(t, x, y, z);
    } else {
        if(n != 2 || !lua_isnumber(L, 1) || !lua_isnumber(L, 2)) {
            lua_pushstring(L, "move_to(x, z)");
            lua_error(L);
            return 0;
        }

        auto x = lua_tonumber(L, 1);
        auto y = lua_tonumber(L, 2);
        auto z = 0.0f;
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
int lua_cut_to(lua_State *L) {
    int n = lua_gettop(L);
    if(t.mode == turtle::mill) {
        if(n != 4 || ( !lua_isnumber(L, 1) && !lua_isnumber(L, 2) && !lua_isnumber(L, 3) ) || !lua_isnumber(L, 4)) {
            lua_pushstring(L, "cut_to(x, y, z, f)");
            lua_error(L);
            return 0;
        }

        auto x = lua_isnil(L, 1) ? t.pos.x : lua_tonumber(L, 1);
        auto y = lua_isnil(L, 2) ? t.pos.y : lua_tonumber(L, 2);
        auto z = lua_isnil(L, 3) ? t.pos.z : lua_tonumber(L, 3);
        auto f = lua_tonumber(L, 4);
        cut_to(t, x, y, z, f);
    } else {
        if(n != 2 || !lua_isnumber(L, 1) || !lua_isnumber(L, 2) || !lua_isnumber(L, 3)) {
            lua_pushstring(L, "cut_to(x, z, f)");
            lua_error(L);
            return 0;
        }

        auto x = lua_tonumber(L, 1);
        auto y = lua_tonumber(L, 2);
        auto z = 0.0f;
        auto f = lua_tonumber(L, 3);
        cut_to(t, x, y, z, f);
    }

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
int lua_arc_to(lua_State *L) {
    int n = lua_gettop(L);
    if(n != 8 || ( !lua_isnumber(L, 1) && !lua_isnumber(L, 2) && !lua_isnumber(L, 3) ) || ( !lua_isnumber(L, 5) && !lua_isnumber(L, 6) ) || !lua_isnumber(L, 8)) {
        lua_pushstring(L, "arc_to(x, y, z, dir, i, j, turns, f)");
        lua_error(L);
        return 0;
    }

    auto x = lua_isnil(L, 1) ? t.pos.x : lua_tonumber(L, 1);
    auto y = lua_isnil(L, 2) ? t.pos.y : lua_tonumber(L, 2);
    auto z = lua_isnil(L, 3) ? t.pos.z : lua_tonumber(L, 3);
    auto dir = lua_isnil(L, 4) ? 1.0f : lua_tonumber(L, 4);
    auto i = lua_isnil(L, 5) ? 0.0f : lua_tonumber(L, 5);
    auto j = lua_isnil(L, 6) ? 0.0f : lua_tonumber(L, 6);
    auto turns = lua_isnil(L, 7) ? 1.0f : lua_tonumber(L, 7);
    auto f = lua_tonumber(L, 8);
    arc_to(t, x, y, z, dir, i, j, turns, f);

    return 0;
}
int lua_turn_to(lua_State *L) {
    int n = lua_gettop(L);
    if(n != 1 || !lua_isnumber(L, 1)) {
        lua_pushstring(L, "turn_to(degrees)");
        lua_error(L);
        return 0;
    }

    auto degrees = lua_tonumber(L, 1);
    turn_to(t, degrees);

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
int lua_pos(lua_State *L) {
    lua_newtable(L);
    lua_pushnumber(L, t.pos.x);
    lua_setfield(L, -2, "x");
    lua_pushnumber(L, t.pos.y);
    lua_setfield(L, -2, "y");
    lua_pushnumber(L, t.pos.z);
    lua_setfield(L, -2, "z");
    return 1;
}
void turtle_open(lua_State* L) {
    lua_register(L, "mode", lua_mode);
    lua_register(L, "move_to", lua_move_to);
    lua_register(L, "move", lua_move);
    lua_register(L, "cut_to", lua_cut_to);
    lua_register(L, "cut", lua_cut);
    lua_register(L, "arc_to", lua_arc_to);
    lua_register(L, "turn_to", lua_turn_to);
    lua_register(L, "turn", lua_turn);
    lua_register(L, "pitch", lua_pitch);
    lua_register(L, "pos", lua_pos);
}


int main(int argc, char* argv[]) {
    po::positional_options_description positional;
    po::options_description options("turtlecam");
    std::vector<std::string> args(argv, argv + argc);
    args.erase(begin(args));

    options.add_options()
        ("help,h", "display this help and exit")
        ("command", po::value<std::vector<std::string>>()->value_name("name")->required(), "Command to execute")
    ;
    positional.add("command", -1);

    try {
        po::variables_map vm;
        store(po::command_line_parser(args).options(options).positional(positional).run(), vm);

        if(vm.count("help")) {
            std::cout << options << "\n";
            return 0;
        }
        notify(vm);

        auto command = vm["command"].as<std::vector<std::string>>();

        lua_State* L = luaL_newstate();
        luaL_openlibs(L);
        turtle_open(L);

        lua_newtable(L);
        for (size_t i = 0; i < command.size(); ++i) {
            lua_pushstring(L, command[i].c_str());
            lua_rawseti(L, -2, i+1);
        }
        lua_setglobal(L, "argv");

        if(luaL_dofile(L, command[0].c_str())) {
            std::cerr << lua_tostring(L, -1) << "\n";
            lua_pop(L, 1);
        }

        lua_close(L);
        L = NULL;
    } catch(const po::error& e) {
        print_exception(e);
        std::cout << options << "\n";
        return 1;
    } catch(const std::exception& e) {
        print_exception(e);
        return 1;
    }

    return 0;
}
