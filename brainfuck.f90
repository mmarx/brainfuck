program brainfuck
  use mod_brainfuck
  implicit none

  integer, parameter :: handle = 42
  character(len=*), parameter :: filename = 'hello.bf'
  character(len=*), parameter :: program = "++++++++++[>+++++++>++++++++++>+++&
&>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
  character, dimension(:), allocatable :: program_array
  integer :: i, length, state
  character :: dummy

  length = 0

  open(unit=handle, file=filename, iostat=state)
  do
     read(unit=handle, fmt='(A)', iostat=state, advance='no') dummy
     length = length + 1
     if(state /= 0) then
        write(*, *) length, dummy, state
        if(state == -2) then
           read(unit=handle, fmt='(/)', iostat=state, advance='no')
        elseif (state == -1) then
           exit
        else
           write(*, *) '>>> ', state
        end if
     end if
  end do
  close(unit=handle)

  allocate(program_array(1:length))

  state = 0

  open(unit=handle, file=filename, iostat=state)  
  do i = 1, length - 1
        read(unit=handle, fmt='(A)', iostat=state, advance='no') program_array(i)
        if(state < 0) write(*, *) 'unexpected EOF at ', i

        write(*, *) '>>> ', program_array(i), ' <<<'
  end do
  close(unit=handle)
     
  call init(data_size = 30000, program_array = program_array)

  call run

  call cleanup

  deallocate(program_array)
end program brainfuck
