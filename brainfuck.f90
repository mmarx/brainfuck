! brainfuck interpreter
! Copyright (C) 2009, 2014, 2016 Maximilian Marx

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

program brainfuck
  use iso_fortran_env, only: iostat_end
  use mod_brainfuck
  implicit none

  integer, parameter :: handle = 42
  character(len=255) :: filename
  character, dimension(:), allocatable :: program_array
  integer :: i, length, state
  character :: dummy

  if (command_argument_count() /= 1) then
     call get_command_argument(0, value=filename)
     write(*, *) 'brainfuck interpreter'
     write(*, *) 'usage: ', trim(filename), ' <program-file>'
     stop
  end if

  call get_command_argument(1, value=filename)
  length = 0

  open(unit=handle, file=filename, iostat=state)
  do
     read(unit=handle, fmt='(A)', iostat=state, advance='no') dummy
     length = length + 1
     if(state /= 0) then
        if (state == iostat_end) then
           exit
        end if
     end if
  end do
  close(unit=handle)

  allocate(program_array(1:length))

  state = 0

  open(unit=handle, file=filename, iostat=state)
  do i = 1, length - 1
        read(unit=handle, fmt='(A)', iostat=state, advance='no') program_array(i)
        if(state == iostat_end) write(*, *) 'unexpected EOF at ', i
  end do
  close(unit=handle)

  call init(data_size = 30000, program_array = program_array)

  call run

  call cleanup

  deallocate(program_array)
end program brainfuck
