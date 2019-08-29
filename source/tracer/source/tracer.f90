module tracer   !possible err: mpi
implicit none
private

integer,parameter::tracer_char_length=100
integer::trace_num=0,trace_level=0,max_trace_num=100
character(len=tracer_char_length),allocatable::trace_items(:),expand_temp(:)
logical(kind=1)::trace_realtime_tag=.false.,trace_enabled=.false.,trace_print_tag=.false.

interface trace
	module procedure add_trace
	module procedure end_trace
end interface


public:: enable_tracer,print_tracer,print_tracer_end,trace_realtime,print_trace,trace,trace_test
contains

subroutine enable_tracer()

	allocate(trace_items(max_trace_num))
	trace_enabled=.true.

end subroutine

subroutine print_tracer()

	trace_print_tag=.true.

end subroutine

subroutine print_tracer_end()

	trace_print_tag=.false.

end subroutine

subroutine trace_realtime(filename)

	character(len=*),intent(in)::filename
	integer::i

	if(trace_enabled)then
		open(777,file=trim(filename))
		rewind(777)
		write(777,*)'The trace of current position is:'
		do i=1,trace_num
			write(777,*)repeat('-',2*i)//trim(trace_items(i))
		end do
		trace_realtime_tag=.true.
	end if

end subroutine

subroutine add_trace(info)

	character(len=*),intent(in)::info
	integer::point_pos

	if(trace_enabled)then
		if(len(trim(info))>tracer_char_length)then
			write(*,*)'Length of input string(',len(trim(info)),' should <= handle_char_length(',tracer_char_length,')'
		else
			if (trace_num==0) then
				trace_num=trace_num+1
				trace_items(trace_num)=info
			else
				if(trim(get_head(trace_items(trace_num)))==trim(get_head(trim(info)))) then
					trace_items(trace_num)=info
					if(trace_realtime_tag) backspace(777)
				else
					trace_level=trace_level+1
					if(trace_num==max_trace_num)then  !exceeds boundary
						allocate(expand_temp(max_trace_num))
						expand_temp=trace_items
						deallocate(trace_items)
						allocate(trace_items(2*max_trace_num))
						trace_items(1:max_trace_num)=expand_temp
						deallocate(expand_temp)
						max_trace_num=2*max_trace_num
					end if
					trace_num=trace_num+1
					trace_items(trace_num)=info
				end if
			end if
			if(trace_realtime_tag) write(777,*)repeat('-',2*trace_level)//trim(trace_items(trace_num))
			if(trace_print_tag)write(*,*)repeat('-',2*trace_level)//trim(trace_items(trace_num))
		end if
	end if

end subroutine

subroutine end_trace()

	if(trace_enabled)then
		trace_level=trace_level-1
		trace_num=trace_num-1
		if(trace_realtime_tag) backspace(777)
		if(trace_num<0)then
			write(*,*)'End_trace not paired with trace.'
			stop
		end if
	end if

end subroutine

subroutine print_trace()

	integer::i

	if(trace_enabled)then
		write(*,*)'---------------------------------'
		write(*,*)'The trace of current position is:'
		do i=1,trace_num
			write(*,*)repeat('-',2*i)//trim(trace_items(i))
		end do
		write(*,*)'---------------------------------'
	end if

end subroutine

character(len=tracer_char_length) function get_head(full) result(head)

	character(len=*),intent(in)::full
	integer::point_pos

	if(len(full)>tracer_char_length)then
		write(*,*)'Length of input string(',len(full),') should <= handle_char_length(',tracer_char_length,')'
		stop
	else
		point_pos=index(full,'.')
		if(point_pos>0) then
			head=full(1:point_pos)
		else
			head=full
		end if
	end if

end function

subroutine trace_test()

	call enable_tracer()
	call print_tracer()
	call trace('1.a')
	call trace('1.b')
	call trace('11.a')
	call trace('11.b')
	call trace('111.a')
	call end_trace()
	call trace('11.c')
	call end_trace()
	call trace('12.a')
	call trace('12.b')
	call trace('121.a')
	call trace('121.b')
	call end_trace()
	call trace('12.c')
	call end_trace()
	call trace('1.c')
	call end_trace()
end subroutine

end module
