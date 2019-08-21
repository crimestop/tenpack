module ecp_handler   !possible err: mpi
implicit none
private

integer,parameter::handle_char_length=100
integer::ecp_num=0,max_ecp_num=100,ecp_level=0,max_ecp_level=20
character(len=handle_char_length),allocatable::ecp_items(:),expand_temp(:)
integer,allocatable::ecp_counter(:,:),expand_temp2(:,:)
logical(kind=1),allocatable::catched(:),expand_temp3(:)
logical(kind=1)::ecp_handler_enabled=.false.

interface catch
	module procedure catch_arg
	module procedure catch_noarg
end interface

public:: enable_ecp_handler,try,catch,throw,end_try
contains

subroutine enable_ecp_handler()
implicit none

	allocate(ecp_items(max_ecp_num))
	allocate(ecp_counter(max_ecp_num,max_ecp_level))
	allocate(catched(max_ecp_level))
	ecp_handler_enabled=.true.

end subroutine

logical function start_with(full,head) result(res)
implicit none

	character(len=*),intent(in)::full,head
	integer::len_head

	res=.false.
	if(len(full)>len(head))then
		if(full(1:len(head)+1)==(head//'.')) res=.true.
	else if(len(full)==len(head)) then
		if(head==full) res=.true.
	end if

end function

subroutine try()
implicit none

	if(ecp_handler_enabled)then
		ecp_level=ecp_level+1
		if (ecp_level>max_ecp_level) then  !exceeds boundary
			allocate(expand_temp2(max_ecp_num,max_ecp_level))
			expand_temp2=ecp_counter
			deallocate(ecp_counter)
			allocate(ecp_counter(max_ecp_num,2*max_ecp_level))
			ecp_counter=0
			ecp_counter(:,1:max_ecp_level)=expand_temp2
			deallocate(expand_temp2)

			allocate(expand_temp3(max_ecp_level))
			expand_temp3=catched
			deallocate(catched)
			allocate(catched(2*max_ecp_level))
			catched(1:max_ecp_level)=expand_temp3
			deallocate(expand_temp3)

			max_ecp_level=2*max_ecp_level
		end if
		catched(ecp_level)=.false.
	end if

end subroutine

logical function catch_arg(exception) result(exist)
implicit none

	character(len=*), intent(in) :: exception
	integer::i

	if(ecp_handler_enabled)then
		catched(ecp_level)=.true.
		exist=.false.
		do i=1,ecp_num
			if(ecp_counter(i,ecp_level)>0)then
				if(start_with(trim(ecp_items(i)),trim(exception)))then
					exist=.true.
					ecp_counter(i,ecp_level)=0
				end if
			end if
		end do 
	else
		exist=.false.
	end if
	
end function

logical function catch_noarg() result(exist)
implicit none

	if(ecp_handler_enabled)then
		catched(ecp_level)=.true.
		exist=any(ecp_counter(1:ecp_num,ecp_level)>0)
		ecp_counter(:,ecp_level)=0
	else
		exist=.false.
	end if
	
end function

subroutine end_try()
implicit none
	
	integer::i,throw_level

	if(ecp_handler_enabled)then
		throw_level=0
		do i=ecp_level-1,1,-1
			if(.not.catched(i))then
				throw_level=i
				exit
			end if
		end do
		if(throw_level>0)then
			ecp_counter(:,throw_level)=ecp_counter(:,throw_level)+ecp_counter(:,ecp_level)
		end if
		ecp_level=ecp_level-1

		if(throw_level<0)then
			write(*,*)'End_try not paired with try.'
			stop
		end if
	end if

end subroutine

subroutine throw(exception)
implicit none

	character(len=*), intent(in) :: exception
	integer::i,throw_level
	logical::exist

	if(ecp_handler_enabled)then
		do i=ecp_level,1,-1
			throw_level=i
			if(.not.catched(throw_level)) exit
		end do
		if(throw_level>0)then
			exist=.false.
			do i=1,ecp_num
				if(trim(ecp_items(i))==trim(exception))then
					exist=.true.
					ecp_counter(i,throw_level)=ecp_counter(i,throw_level)+1
					exit
				end if
			end do 
			if(.not.exist)then
				if(ecp_num==max_ecp_num)then  !exceeds boundary
					allocate(expand_temp(max_ecp_num))
					expand_temp=ecp_items
					deallocate(ecp_items)
					allocate(ecp_items(2*max_ecp_num))
					ecp_items(1:max_ecp_num)=expand_temp
					deallocate(expand_temp)

					allocate(expand_temp2(max_ecp_num,max_ecp_level))
					expand_temp2=ecp_counter
					deallocate(ecp_counter)
					allocate(ecp_counter(2*max_ecp_num,max_ecp_level))
					ecp_counter=0
					ecp_counter(1:max_ecp_num,:)=expand_temp2
					deallocate(expand_temp2)

					max_ecp_num=2*max_ecp_num
				end if
				ecp_num=ecp_num+1
				ecp_items(ecp_num)=exception
				ecp_counter(ecp_num,throw_level)=1
			end if
		end if
	end if

end subroutine

end module ecp_handler
