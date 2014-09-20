! brainfuck interpreter
! Copyright (C) 2009, 2014 Maximilian Marx

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

module mod_brainfuck
  implicit none

  private
  public :: init, run, cleanup

  integer, dimension(:), allocatable :: data
  character, dimension(:), allocatable :: program
  integer :: data_pointer, instruction_pointer, program_length

contains
  subroutine init(data_size, program_array)
    integer, intent(in) :: data_size
    character, dimension(1:), intent(in) :: program_array

    integer :: i

    program_length = ubound(program_array, 1)

    allocate(data(1:data_size))
    allocate(program(1:program_length))

    program = program_array

    data = 0
    data_pointer = 1
    instruction_pointer = 1
  end subroutine init

  subroutine cleanup()
    deallocate(data)
    deallocate(program)
  end subroutine cleanup

  subroutine run()
    character :: tmp
    integer :: start

    do
       select case(program(instruction_pointer))
       case('<')
          data_pointer = data_pointer - 1
       case('>')
          data_pointer = data_pointer + 1
       case('-')
          data(data_pointer) = data(data_pointer) - 1
       case('+')
          data(data_pointer) = data(data_pointer) + 1
       case(',')
          read(*, *) tmp
          data(data_pointer) = ichar(tmp)
       case('.')
          write(*, fmt='(A)', advance='no') char(data(data_pointer))
       case('[')
          if(data(data_pointer) == 0) then
             start = instruction_pointer + 1
             instruction_pointer = first(program(start:program_length), ']')
          end if
       case(']')
          if(data(data_pointer) /= 0) then
             instruction_pointer = instruction_pointer + 1 &
                  - first(program(instruction_pointer:1:-1), '[')
          end if
       case default
       end select

       instruction_pointer = instruction_pointer + 1
       if(instruction_pointer > program_length) exit
    end do

    write(*, *)
  end subroutine run

  integer pure function first(haystack, needle)
    character, dimension(:) :: haystack
    character :: needle
    intent(in) :: haystack, needle

    integer :: i
    first = -1

    do i = lbound(haystack, 1), ubound(haystack, 1)
       if(haystack(i) == needle) then
          first = i
          return
       end if
    end do
  end function first
end module mod_brainfuck
