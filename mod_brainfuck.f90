module mod_brainfuck
  implicit none

  integer, dimension(1:30000) :: data
  integer :: ptr, ip
  
contains
  integer pure function next(haystack, needle, start, backwards)
    character(len=*), intent(in) :: haystack
    character, intent(in) :: needle
    integer, intent(in) :: start
    logical, optional, intent(in) :: backwards
    logical :: bwards
    integer :: i, strt, end, step
    
    next = -1

    if(present(backwards)) bwards = backwards
    
    if(bwards) then
       strt = ip
       end = 1
       step = -1
    else
       strt = start
       end = len(haystack)
       step = 1
    end if

    do i = strt, end, step
       if(haystack(i:i) == needle) then
          next = i
          return
       end if
    end do
  end function next
      
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
!                ip = next(program, ']', ip + 1)
             end if
          case(']')
             if(data(ptr) /= 0) then
!                ip = next(program, '[', ip, .true.)
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
