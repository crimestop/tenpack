program main
use ecp_handler
implicit none

call enable_tracer()
call enable_ecp_handler()
call trace_realtime('abd.dat')
call do_test()
contains


subroutine do_test()
implicit none
call trace('do_test.start')
call try()
call test_1()
if(catch('dont_catch_me'))then
	write(*,*)'found exception dont_catch_me'
end if 
call end_try()

call trace('do_test.finish1')

call try()
call test_2()
if(catch('math.overflow'))then
	write(*,*)'found exception math.overflow'

	call try()
	call fix()
	if(catch('fix.failed'))then
		write(*,*)'fix.failed'
	end if
	call end_try()

end if 
if(catch('math.nan'))then
	write(*,*)'found exception math.nan'
end if 
if(catch('math'))then
	write(*,*)'found other math exception'
end if 
if(catch())then
	write(*,*)'found other exception'
end if 
call end_try()

call trace('do_test.finish2')

call trace()

end subroutine

subroutine test_1()
implicit none

call trace('test_1.start')

call test_1_1()
call trace('test_1.finish1')

call try()
call test_1_2()
if(catch('catch_me'))then
	write(*,*)'found exception catch_me'
end if
call end_try()

call trace('test_1.finish2')
call print_trace()

call trace()

end subroutine

subroutine test_1_1()
implicit none

call trace('test_1_1.start')

call trace()

end subroutine

subroutine test_1_2()
implicit none

call trace('test_1_2.start')
call print_trace()
call throw('dont_catch_me')
call throw('catch_me')

call trace()

end subroutine

subroutine test_2()
implicit none

call trace('test_2.start')
call throw('math.overflow')
call throw('math.underflow')
call throw('math.nan')
call throw('boom')
call print_trace()
stop

call trace()

end subroutine

subroutine fix()
implicit none

call throw('fix.failed')

end subroutine

end program
