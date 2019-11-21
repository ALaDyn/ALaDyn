/*******************************************************************************************************
 *                            Copyright 2008-2019  The ALaDyn Collaboration                            *
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

extern "C" {

void memaddr_(void* var, unsigned long long int* addr)
{
  *addr = (unsigned long long int)var;
  return;
}
}
