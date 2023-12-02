#include <algorithm>
#include <cstdio>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using std::vector;
using std::string;

struct HandfulOfCubes {
    int red = 0;
    int green = 0;
    int blue = 0;
};

struct Game {
    int id;
    vector<HandfulOfCubes> configurations;
};

Game parse_line(string line) {
    Game game;

    std::istringstream iss(line);

    // scan to "Game "
    iss.ignore(2000, ' ');
    iss >> game.id;

    // ignore ": "
    iss.ignore(2);

    bool gameloop = true;

    while (gameloop) {

        // scan a bunch of configurations (until a ;)
        HandfulOfCubes configuration;

        while (true) {

            int amount;
            string color;

            iss >> amount >> color;

            if (iss.fail()) {
                gameloop = false;
                break;
            }

            if (color.substr(0, 3) == "red") {
                configuration.red += amount;
            } else if (color.substr(0, 4) == "blue") {
                configuration.blue += amount;
            } else if (color.substr(0, 5) == "green") {
                configuration.green += amount;
            }

            if (color.rfind(';') == color.length() - 1) {
                // end of this configuration
                break;
            }
        }

        game.configurations.push_back(configuration);
    }

    return game;
}

void debugprint(vector<Game> games) {
    for (const auto& game: games) {
        std::cout << game.id << " ";

        for (const auto& config: game.configurations) {
            std::cout << config.red << " " << config.green << " " << config.blue << "; ";
        }

        std::cout << std::endl;
    }
}

bool isPossiblePart1(HandfulOfCubes configuration) {
    return configuration.red <= 12 && configuration.green <= 13 && configuration.blue <= 14;
}

int main() {
    vector<Game> games;

    string line;

    while (std::getline(std::cin, line)) {
        Game game = parse_line(line);
        games.push_back(game);
    }

    // part 1
    int sum1 = 0;
    for (Game game : games) {
        bool possible = true;

        for (HandfulOfCubes configuration : game.configurations) {
            possible = possible && isPossiblePart1(configuration);
        }

        if (possible) {
            sum1 += game.id;
        }
    }

    std::cout << "part 1: " << sum1 << std::endl;

    // part 2
    int sum2 = 0;

    for (Game game : games) {
        int maxred = 0;
        int maxgreen = 0;
        int maxblue = 0;

        for (HandfulOfCubes configuration : game.configurations) {
            maxred = std::max(maxred, configuration.red);
            maxgreen = std::max(maxgreen, configuration.green);
            maxblue = std::max(maxblue, configuration.blue);
        }

        int power = maxred * maxgreen * maxblue;

        sum2 += power;
    }

    std::cout << "part 2: " << sum2 << std::endl;

    return 0;
}
