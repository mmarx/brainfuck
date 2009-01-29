module mod_brainfuck
  implicit none

  integer, dimension(1:30000) :: data
  integer :: ptr, ip
  
contains 
  subroutine parse(program)
    character(len=*), intent(in) :: program
    character :: cmd, tmp
    integer :: length

    data = 0
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
             write(*, fmt='(A)', advance='no') char(data(ptr))
          case(',')
             read(*, *) tmp
             data(ptr) = ichar(tmp)
          case('[')
             if(data(ptr) == 0) then
                ip = scan(program((ip + 1):length), ']')
             end if
          case(']')
             if(data(ptr) /= 0) then
                ip = scan(program(1:ip), '[', .true.)
             end if
          case default
             write(*, *) 'got unknown: ', cmd
       end select

       ip = ip + 1

       if(ip > length) exit
    end do

    write(*, *)
  end subroutine parse

end module mod_brainfuck
