 !*****************************************************************************************************!
 !             Copyright 2008-2018 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
 !*****************************************************************************************************!

 !*****************************************************************************************************!
 !  This file is part of ALaDyn.                                                                       !
 !                                                                                                     !
 !  ALaDyn is free software: you can redistribute it and/or modify                                     !
 !  it under the terms of the GNU General Public License as published by                               !
 !  the Free Software Foundation, either version 3 of the License, or                                  !
 !  (at your option) any later version.                                                                !
 !                                                                                                     !
 !  ALaDyn is distributed in the hope that it will be useful,                                          !
 !  but WITHOUT ANY WARRANTY; without even the implied warranty of                                     !
 !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                                      !
 !  GNU General Public License for more details.                                                       !
 !                                                                                                     !
 !  You should have received a copy of the GNU General Public License                                  !
 !  along with ALaDyn.  If not, see <http://www.gnu.org/licenses/>.                                    !
 !*****************************************************************************************************!


 integer FFTW_FORWARD,FFTW_BACKWARD
 parameter (FFTW_FORWARD=-1,FFTW_BACKWARD=1)

 integer FFTW_REAL_TO_COMPLEX,FFTW_COMPLEX_TO_REAL
 parameter (FFTW_REAL_TO_COMPLEX=-1,FFTW_COMPLEX_TO_REAL=1)

 integer FFTW_ESTIMATE,FFTW_MEASURE
 parameter (FFTW_ESTIMATE=0,FFTW_MEASURE=1)

 integer FFTW_OUT_OF_PLACE,FFTW_IN_PLACE,FFTW_USE_WISDOM
 parameter (FFTW_OUT_OF_PLACE=0)
 parameter (FFTW_IN_PLACE=8,FFTW_USE_WISDOM=16)

 integer FFTW_THREADSAFE
 parameter (FFTW_THREADSAFE=128)

 ! Constants for the MPI wrappers:
 integer FFTW_TRANSPOSED_ORDER, FFTW_NORMAL_ORDER
 integer FFTW_SCRAMBLED_INPUT, FFTW_SCRAMBLED_OUTPUT
 parameter(FFTW_TRANSPOSED_ORDER=1, FFTW_NORMAL_ORDER=0)
 parameter(FFTW_SCRAMBLED_INPUT=8192)
 parameter(FFTW_SCRAMBLED_OUTPUT=16384)
 integer FFTW_REDFT00
