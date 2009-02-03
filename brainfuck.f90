program brainfuck
  use mod_brainfuck
  implicit none

  character(len=*), parameter :: program = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
  character, dimension(:), allocatable :: program_array
  integer :: i, length

  length = len(program)

  allocate(program_array(1:length))
  
  do i = 1, length
     program_array(i) = program(i:i)
  end do

  call init(data_size = 30000, program_array = program_array)

  call run

  call cleanup

  deallocate(program_array)
end program brainfuck
