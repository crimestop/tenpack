module timer
use type_unidic
use string
use error
use tools
implicit none
private

type(unidic):: name_order
integer,parameter::max_elem=100
real(8)::start_time(max_elem)=0d0
real(8)::cumu_time(max_elem)=0d0
character(len=max_char_length)::names(max_elem)=''
integer::state(max_elem)=0 ! 0 for not used, 1 for started, 2 for finished
real(8)::get_time
external::get_time

public timer_start,timer_end,timer_print,timer_restart
contains

subroutine timer_start(name)

	character(len=*),intent(in)::name
	integer::val
	real(8)::time

	val = name_order%val(trim(name))
	if (val==0) then
		call name_order%add(trim(name),val)
		names(val)=name
	end if
	if(state(val)==1) then
		call wc_error_stop('benchmark.timer_start', 'timer for '//trim(name)//' has already been started')
	end if
	state(val)=1
	time=get_time() 
	start_time(val)=time

end subroutine

subroutine timer_end(name)

	character(len=*),intent(in)::name
	integer::val
	real(8)::time

	val = name_order%val(trim(name))
	if(val==0) then
		call wc_error_stop('benchmark.benchmark_end', 'timer for '//trim(name)//' has not been started')
	end if
	if(state(val)==2) then
		call wc_error_stop('benchmark.benchmark_end', 'timer for '//trim(name)//' has not been started')
	end if
	time=get_time() 
	cumu_time(val)=cumu_time(val)+(time-start_time(val))
	start_time(val)=0d0
	state(val)=2

end subroutine

subroutine timer_print()

	integer::i
	character(len=15)::item_name
	
	call writemess('')
	call writemess('================================')
	item_name='Timer'
	call writemess(item_name//'Total time(s)')
	do i=1, max_elem
		if (state(i)>0) then
			item_name=names(i)
			call writemess(item_name//str(cumu_time(i)))
		end if
	end do
	call writemess('================================')
	call writemess('')

end subroutine

subroutine timer_restart()

	integer::i

	start_time=0d0
	cumu_time=0d0
	do i=1, max_elem
		if (state(i)==1) state(i)=2
	end do

end subroutine

end module
