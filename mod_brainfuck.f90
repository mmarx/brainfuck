module mod_brainfuck
  implicit none

  private
  public :: init, run, cleanup

  integer, dimension(:), allocatable :: data
  character, dimension(:), allocatable :: program
  integer :: data_pointer, instruction_pointer, program_length
  
contains 
  subroutine init(data_size, program_string)
    integer, intent(in) :: data_size
    character(len=*), intent(in) :: program_string

    integer :: i

    program_length = len(program_string)

    allocate(data(1:data_size))
    allocate(program(1:program_length))

    do i = 1, program_length
       program(i) = program_string(i:i)
    end do

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
