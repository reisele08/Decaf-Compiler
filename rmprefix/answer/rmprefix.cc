#include <iostream>
#include <string>
#include <cstdlib>

int main() {
    for (std::string line; std::getline(std::cin, line);) {
        if(line.size() < 0){
            exit(EXIT_FAILURE);
        }
        std::cout << line.erase(0, line.find_first_not_of("\t\n\v\f\r ")) << std::endl;

    }
	exit(EXIT_SUCCESS);
}
