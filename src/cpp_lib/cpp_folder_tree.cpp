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

#include <filesystem>

extern "C" {
void create_folder_(char* folderName, size_t len) {
  std::string fname(folderName, 0, len);
  std::filesystem::create_directories(fname);
}

void check_folder_empty_(int* isempty, char* folderName, size_t len) {
  std::string fname(folderName, 0, len);
  // Trim trailing whitespace (Fortran strings may be padded)
  fname.erase(fname.find_last_not_of(" \t\n\r") + 1);

  if (!std::filesystem::exists(fname) || !std::filesystem::is_directory(fname)) {
    *isempty = 1; // Treat non-existent or non-directory as empty
    return;
  }

  // Check if directory is empty
  *isempty = std::filesystem::is_empty(fname) ? 1 : 0;
}
}
