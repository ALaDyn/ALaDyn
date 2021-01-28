/*******************************************************************************************************
 *                            Copyright 2008-2020  The ALaDyn Collaboration                            *
 *******************************************************************************************************

 *******************************************************************************************************
 *  This file is part of ALaDyn.                                                                       *
 *                                                                                                     *
 *  ALaDyn is free software: you can redistribute it and/or modify                                     *
 *  it under the terms of the GNU General Public License as published by                               *
 *  the Free Software Foundation, either version 3 of the License, or                                  *
 *  (at your option) any later version.                                                                *
 *                                                                                                     *
 *  ALaDyn is distributed in the hope that it will be useful,                                          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of                                     *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                                      *
 *  GNU General Public License for more details.                                                       *
 *                                                                                                     *
 *  You should have received a copy of the GNU General Public License                                  *
 *  along with ALaDyn.  If not, see <http://www.gnu.org/licenses/>.                                    *
 ******************************************************************************************************/

#if defined(USE_BOOST)
#include <boost/filesystem.hpp>
#include <cstring>

extern "C" {
void create_folder_(char* folderName, size_t len)
{
  std::string fname(folderName, 0, len);
  boost::filesystem::create_directories(fname);
}

void check_folder_empty_(bool* pathIsempty, char* folderName, size_t len)
{
  std::string fname(folderName, 0, len);
  boost::filesystem::path folderPath(fname);
  *pathIsempty = true;
  bool pathExists = boost::filesystem::exists(folderPath);

  if (pathExists){
    *pathIsempty = boost::filesystem::is_empty(folderPath);
  }

}
}

#elif defined(USE_FILESYSTEM)
#include <filesystem>

extern "C" {
void create_folder_(char* folderName, size_t len) {
  std::string fname(folderName, 0, len);
  std::filesystem::create_directories(fname);
}
}
#elif defined(_WIN32)
#include <direct.h>
#include <stdio.h>

extern "C"
{
void create_folder_(char* folderName, size_t len) {
  if (_mkdir(folderName) != 0) {
    printf("Problem creating directory %s!\n", folderName);
  }
}

void check_folder_empty_(bool* pathIsempty, char* folderName, size_t len){
  *pathIsempty = false;
}

}
#else

extern "C" {
void create_folder_(char* folderName, size_t len) {}
}
#endif
