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
