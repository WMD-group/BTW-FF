!This script converts tinker output .xyz files into general format .xyz
!files readable by the majority of visualisation and editing software
!programmes.

!To run just type in the name of the tinker output file without the .xyz
!extension and out.xyz will be generated.

   ! This program is free software: you can redistribute it and/or modify
   ! it under the terms of the GNU General Public License as published by
   ! the Free Software Foundation, either version 3 of the License, or
   ! (at your option) any later version.

   ! This program is distributed in the hope that it will be useful,
   ! but WITHOUT ANY WARRANTY; without even the implied warranty of
   ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   ! GNU General Public License for more details.

   ! You should have received a copy of the GNU General Public License
   ! along with this program.  If not, see <http://www.gnu.org/licenses/>. 
   !Written by Jessica K. Bristow and Davide Tiana, University of Bath
   !Made open access on Tuesday 10th June 2014

Program conversion
   Implicit none
   
      Character(20) :: file_name, file_name2
      Character(30) :: connect
      Character(60) :: line1
      Integer :: atom_class, atom_num, i_err
      Real :: x, y, z   
      character(3) :: atom_name
      logical :: lexist
      character(4),parameter :: ext='.xyz'



	Write(*,*)'Enter the name of the tinker xyz file you wish to convert to a general .xyz file. Do not include .xyz'

	Read (*,*)file_name


		file_name2 = trim(file_name)//ext

		OPEN(unit=1, file=file_name2, status='old', action='read', iostat=i_err)
		INQUIRE(unit=1, exist=lexist)
		IF (i_err .ne. 0) THEN
		Write(*,*)'File does not exist, computer says no :-('
		STOP
		END IF

		OPEN(unit=4, file='out.xyz', status='replace', action='write', iostat=i_err)


		IF (i_err .ne. 0) Then
 	        write(*,*)'error in opening file'
                STOP
		END IF 

	Read(1,'(A60)', iostat=i_err) line1 

	Write(4,'(A60)') line1
	Write(4,'(A60)') '  '

    			DO
			Read(1,*,iostat=i_err) atom_num, atom_name, x, y, z, atom_class, connect
   			IF (i_err .ne. 0) EXIT



			Write(4,*) atom_name, x, y, z
			END DO

CLOSE(unit=1, iostat=i_err)
CLOSE(unit=4, iostat=i_err)


	IF (i_err .ne. 0) Then
 	write(*,*)'error in closing file'
 	STOP
	END IF
Write(*,*)'out.xyz has been generated and can be read by the majority of visualisation programmes. Thank you, come again!'

End program conversion
