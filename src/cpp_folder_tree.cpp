/****************************************************************************************************
             Copyright 2008-2016 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni
                                                Alberto Marocchino
*****************************************************************************************************
*****************************************************************************************************
  This file is part of ALaDyn.

  ALaDyn is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  ALaDyn is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with ALaDyn.  If not, see <http://www.gnu.org/licenses/>.
****************************************************************************************************/


#include <cstring>
#include <boost/filesystem.hpp>

extern "C" {

#if defined (__xlC__)
  void create_folder(char * folderName, size_t len) {
#elif defined (_MSC_VER)
  void CREATE_FOLDER(char * folderName, size_t len) {
#else
  void create_folder_(char * folderName, size_t len) {
#endif
    std::string fname(folderName, 0, len);
    if (!boost::filesystem::exists(fname)) {
      boost::filesystem::create_directories(fname);
    }
    /*
    else {
      std::string oldFolder = fname+".old";
      boost::filesystem::rename(fname, oldFolder);
      boost::filesystem::create_directories(fname);
    }
    */
  }


}
