/****************************************************************************************************
*                           Copyright 2008-2018  The ALaDyn Collaboration                           *
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

#ifdef _WIN32
#include <process.h>
#else
#include <sys/types.h>
#include <unistd.h>
#define _getpid(x) getpid(x)
#endif
#include <cstdio>


extern "C" {

  void gdbattach_() {
    volatile int wli = 0;
    printf("PID %d ready for attach\n", _getpid());
    fflush(stdout);
    while (wli == 0)
        sleep(5);
    return;
  }
}
