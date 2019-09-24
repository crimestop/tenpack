module timer
use type_unidic
use string
use error
implicit none
private

type(unidic):: name_order
integer,parameter::max_elem=100
real(8)::start_time(max_elem)=0d0
real(8)::cumu_time(max_elem)=0d0
character(len=max_char_length)::names(max_elem)=''
integer::state(max_elem)=0 ! 1 for started, 2 for ended,  0 for not exist
real(8)::get_time
external::get_time

interface timer_refresh
	module procedure timer_refresh_all
	module procedure timer_refresh_name
end interface

public timer_start,timer_end,timer_print,timer_refresh,timer_get
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
		call wc_error_stop('timer.start', 'timer for '//trim(name)//' has already been started')
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
		call wc_error_stop('timer.end', 'timer for '//trim(name)//' has not been started')
	end if
	if(state(val)==0) then
		call wc_error_stop('timer.end', 'timer for '//trim(name)//' has been ended')
	end if
	time=get_time() 
	cumu_time(val)=cumu_time(val)+(time-start_time(val))
	start_time(val)=0d0
	state(val)=2

end subroutine

function timer_get(name) result(time)

	character(len=*),intent(in)::name
	integer::val
	real(8)::time,time2

	val = name_order%val(trim(name))
	if(val==0) then
		call wc_error_stop('timer.get', 'timer for '//trim(name)//' has not been started')
	end if
	if(state(val)/=0) then
		time2=get_time() 
		time=cumu_time(val)+(time2-start_time(val))
	else
		time=cumu_time(val)
	end if
	
end function

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

subroutine timer_refresh_name(name)

	character(len=*),intent(in)::name
	integer::val

	val = name_order%val(trim(name))
	if(val==0) then
		call wc_error_stop('timer.restart', 'timer for '//trim(name)//' has not been started')
	end if
	cumu_time(val)=0d0
	start_time(val)=0d0
	state(val)=0

end subroutine

subroutine timer_refresh_all()

	start_time=0d0
	cumu_time=0d0
	state=0

end subroutine

end module
