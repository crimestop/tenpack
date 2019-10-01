module timer
use type_unidic
use string
use error
implicit none
private

type time_record
	real(8)::start_time=0d0
	real(8)::cumu_time=0d0
	character(len=max_char_length)::name=''
	logical::started=.false.
end type

type(unidic):: name_order
integer::max_elem=100 ! if elem num > max_elem then max_elem += 100
type(time_record),allocatable::times(:)
real(8),external::get_time

interface timer_refresh
	module procedure timer_refresh_all
	module procedure timer_refresh_name
end interface

public timer_start,timer_end,timer_print,timer_refresh,timer_get
contains

subroutine timer_start(name)

	character(len=*),intent(in)::name
	integer::val
	type(time_record),allocatable::tmp_times(:)

	if(.not. allocated(times))then
		allocate(times(max_elem))
	end if

	val = name_order%val(trim(name))
	if (val==0) then
		call name_order%add(trim(name),val)
		if(val>max_elem) then
			allocate(tmp_times(max_elem+100))
			tmp_times(:max_elem)=times
			deallocate(times)
			call move_alloc(tmp_times,times)
			max_elem=max_elem+100
		end if
		times(val)%name=name
	end if
	if(times(val)%started) then
		call wc_error_stop('timer.start', 'timer for '//trim(name)//' has already been started')
	end if
	times(val)%started=.true.
	times(val)%start_time=get_time()

end subroutine

subroutine timer_end(name)

	character(len=*),intent(in)::name
	integer::val
	real(8)::time

	val = name_order%val(trim(name))
	if(val==0) then
		call wc_error_stop('timer.end', 'timer for '//trim(name)//' has not been started')
	end if
	if(.not. times(val)%started) then
		call wc_error_stop('timer.end', 'timer for '//trim(name)//' has not been started')
	end if
	times(val)%cumu_time=times(val)%cumu_time+(get_time() -times(val)%start_time)
	times(val)%start_time=0d0
	times(val)%started=.false.

end subroutine

function timer_get(name) result(time)

	character(len=*),intent(in)::name
	integer::val
	real(8)::time

	val = name_order%val(trim(name))
	if(val==0) then
		call wc_error_stop('timer.get', 'timer for '//trim(name)//' has not been started')
	end if
	if(times(val)%started) then
		time=times(val)%cumu_time+(get_time() -times(val)%start_time)
	else
		time=times(val)%cumu_time
	end if
	
end function

subroutine timer_print()

	integer::val
	real(8)::time
	character(len=max_char_length)::item_name
	
	call write_message('')
	call write_message('================================')
	item_name='Timer'
	call write_message(item_name//'Total time(s)')
	! do i=1, max_elem
	! 	if (state(i)>0) then
	! 		item_name=names(i)
	! 		call write_message(item_name//str(cumu_time(i)))
	! 	end if
	! end do
	do while(name_order%iterate(item_name,val))
		if(times(val)%started) then
			time=times(val)%cumu_time+(get_time() -times(val)%start_time)
		else
			time=times(val)%cumu_time
		end if
		call write_message(item_name//': '//str(time))
	end do
	call write_message('================================')
	call write_message('')

end subroutine

subroutine timer_refresh_name(name)

	character(len=*),intent(in)::name
	integer::val

	val = name_order%val(trim(name))
	if(val==0) then
		call wc_error_stop('timer.restart', 'timer for '//trim(name)//' has not been started')
	end if
	times(val)%cumu_time=0d0
	times(val)%start_time=0d0
	times(val)%started=.false.

end subroutine

subroutine timer_refresh_all()

	integer::val
	character(len=max_char_length)::item_name

	do while(name_order%iterate(item_name,val))
		times(val)%cumu_time=0d0
		times(val)%start_time=0d0
		times(val)%started=.false.
	end do

end subroutine

end module
