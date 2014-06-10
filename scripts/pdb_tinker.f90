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


Program read_file
Implicit none

CHARACTER(LEN=60) :: line, line2, line4
CHARACTER(len=110) :: line3
CHARACTER(6) :: A
CHARACTER(3) :: atom_name, unk
CHARACTER(6) :: tmp
CHARACTER(6) :: tmp2
CHARACTER(60) :: tmp3, tmp4
Integer :: num, one, i, j 
Real :: X, Y, Z
Integer :: i_err
Character(20) :: file_name
Character(20) :: file_name2
character(4), parameter :: ext='.pdb'
logical :: lexist

!This script generates the format of a tinker input file from a pdb file. It cannot assign periodic connectivity. Atom types present are for those given in the associated published articles but can be edited to your requirements. 

	Write(*,*)'enter the name of the pdb file you want to convert to a tinker xyz file'
	Read(*,*) file_name

		file_name2 = trim(file_name)//ext

			OPEN(unit=1, file=file_name2, status='old', action='read', iostat=i_err)
			INQUIRE (unit=1, exist=lexist)
			IF (i_err .ne. 0) Then
			Write(*,*)'file does not exist, computer says no :-('
			STOP
			END IF


			OPEN(unit=2, status='scratch', iostat=i_err)
			OPEN(unit=3, status='scratch', iostat=i_err)
			OPEN(unit=4, file='output_tinker.xyz', status='replace', action ='write', iostat=i_err)

			!all files opened. files 2 and 3 = temporary and so are not named
				write(*,*) file_name2

				IF (i_err .ne. 0) Then
				write(*,*)'error in opening file'
				STOP
				END IF

					DO
 					Read(1,'(A60)',iostat=i_err) line


!read lines as 60 length character strings to identify those beginning with CONECT and HETATM 
				  IF (i_err .ne. 0) EXIT
				  READ(line,*) A
				  IF (A == 'HETATM') THEN
    
					  READ(line,*) A, num, atom_name, X, Y, Z 
!!!!READ(line,*) A, num, atom_name, unk, one, X, Y, Z 
!!!!Replace with the alternative line if the pdb has been generated in Mercury



                SELECT CASE (atom_name)
                    CASE('Zn1')
                    i = 172
                    CASE('Zr1')
                    i = 192
                    CASE('Cu1')
                    i = 185
                    CASE('O1')
                    i = 170
                    CASE('O2')
                    i = 171
                    CASE('O3')
                    i = 75
                    CASE('C1')
                    i = 2
                    CASE('C2')
                    i = 3
                    CASE('C3')
                    i = 902
                    CASE('C4')
                    i = 903
                    CASE('H1')
                    i = 5
                    CASE('H2')
                    i = 21
                 END SELECT

!!!more parameters will need to be added into this select case when assigning own atom type. ENSURE THE SUBJECT OF THE CASE STAEMENT E.G.ZN1 IS APPROPRIATELY LABELLED IN THE .PDB FILE

			Write(2,100) num, atom_name, X, Y, Z, i


		END IF
!unit=2 contains atom types next to atom number, atom name and coordinates

                          IF (A == 'CONECT') THEN
                   
                       Write(3,*) line
                     END IF
                     END DO

!unit=3 contains connectivity of all atoms, which are printed at the bottom of a pdb file 


REWIND(2)
REWIND(3)
!re-set temp files which have been written to

	   
WRITE(4,'(I3, A40)') num, 'Enter the name of your file here'

		DO
                    Read(2,'(A60)',iostat=i_err) line
                    IF (i_err .ne. 0) EXIT
			
                    Read(3,'(A60)',iostat=i_err) line2
                    IF (i_err .ne. 0) EXIT

                    Read(line2,'(A7,A7,A60)',iostat=i_err) tmp, tmp2, tmp3
                    IF (i_err .ne. 0) EXIT
line4 = trim(line)
tmp4 = trim(tmp3)
                        line3 = line4 // tmp4

                 !concatenate string variables and write into file permanent output   
	Write(4,*)line3
		
                    END DO
                    
                   
                   
                   

write(*,*) 'your tinker input has been printed as output_tinker.txt :-)'

CLOSE(unit=2,iostat=i_err)
CLOSE(unit=3,iostat=i_err)
CLOSE(unit=4,iostat=i_err)
CLOSE(unit=1, iostat=i_err)

 100 FORMAT (2x,(I3,2x), (A3,2x), (F11.5,2x), (F11.5,2x), (F11.5,2x),2x, (I3,2x))
   

		IF (i_err .ne. 0) Then
		write(*,*) 'error in closing file'
		STOP
		END IF



End Program read_file
