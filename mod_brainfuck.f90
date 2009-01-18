module mod_brainfuck
  implicit none

  integer, dimension(1:30000) :: data
  integer :: ptr, ip
  
contains
  subroutine parse(program)
    character(len=*), intent(in) :: program
    character :: cmd, tmp
    integer :: length

    length = len(program)
    ip = 1
    do
       cmd = program(ip:ip)
       
       select case(cmd)
          case('<')
             ptr = ptr - 1
          case('>')
             ptr = ptr + 1
          case('+')
             data(ptr) = data(ptr) + 1
          case('-')
             data(ptr) = data(ptr) - 1
          case('.')
             write(*, *) char(data(ptr))
          case(',')
             read(*, *) tmp
             data(ptr) = ichar(tmp)
          case('[')
             write(*, *) 'should loop here.'
          case(']')
             write(*, *) 'should loop here.'
          case default
             write(*, *) 'got unknown: ', cmd
       end select
       
       ip = ip + 1

       if(ip > length) exit
    end do
  end subroutine parse

end module mod_brainfuck
