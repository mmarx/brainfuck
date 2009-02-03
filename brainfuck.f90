program brainfuck
  use mod_brainfuck
  implicit none

  character(len=*), parameter :: program = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

  call init(data_size = 30000, program_string = program)

  call run

  call cleanup
end program brainfuck
