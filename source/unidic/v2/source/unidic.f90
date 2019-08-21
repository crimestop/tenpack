module type_unidic
use error
use string
implicit none
private

type node
	private
	type(node),pointer::next=>null()
	character(len=max_char_length)::key=''
	integer::val=0
end type node

integer,parameter::hash_size=787
integer,parameter::stack_limit=1000

type node_head
	private
	type(node),pointer::first=>null()
end type node_head

type unidic
	private
	type(node_head)::hash_ary(hash_size)
	integer::item_num=0,stack_num=0
	integer::avail_stack(stack_limit)
	contains
	private
	procedure,public::add
	procedure::add_with_val
	procedure,public::del
	procedure::del_with_val
	procedure,public::val
	procedure,public::show
	procedure,public::rename
	procedure,public:: clean
	procedure,public:: print
	procedure,public:: read
	!procedure,public::check_consistency
	final:: clean_dic

end type unidic

interface assignment(=)
	module procedure copy
end interface

public unidic
contains

subroutine clean_dic(U)

	type(unidic),intent(inout)::U

	call U%clean()

end subroutine

subroutine clean(U)

	class(unidic),intent(inout)::U
	type(node),pointer::p,p_n
	integer::i

	U%item_num=0
	U%stack_num=0

	do i=1,hash_size
		if(associated(U%hash_ary(i)%first))then
			p=>U%hash_ary(i)%first			!first node
			U%hash_ary(i)%first=>null()
			do while(associated(p%next))
				p_n=>p%next
				p%next=>p_n%next
				deallocate(p_n)
			end do
			deallocate(p)
		end if
	end do	

end subroutine

subroutine copy(U_out,U_in)

	class(unidic),intent(inout)::U_out
	class(unidic),intent(in)::U_in
	type(node),pointer::p_in,p_out
	integer::i

	call clean(U_out)

	U_out%item_num=U_in%item_num
	U_out%stack_num=U_in%stack_num

	do i=1,hash_size
		if(associated(U_in%hash_ary(i)%first))then
			allocate(U_out%hash_ary(i)%first)
			p_in=>U_in%hash_ary(i)%first
			p_out=>U_out%hash_ary(i)%first
			p_out%val=p_in%val
			p_out%key=p_in%key
			do while(associated(p_in%next))
				allocate(p_out%next)
				p_in=>p_in%next
				p_out=>p_out%next
				p_out%val=p_in%val
				p_out%key=p_in%key
			end do
		end if
	end do

end subroutine

subroutine show(U)

	class(unidic),intent(inout)::U
	type(node),pointer::p
	integer::i

	if(U%item_num==0)then
		write(*,*)'Empty dictionary.'
	else
		write(*,*)'Dictionary with '//trim(str(U%item_num))//' items:'
		do i=1,hash_size
			if(associated(U%hash_ary(i)%first))then
				p=>U%hash_ary(i)%first
				write(*,'(A)',advance='no')'  hash('//trim(str(i))//'):'
				do while(associated(p))
					write(*,'(A)',advance='no')' ('//trim(p%key)//':'//trim(str(p%val))//')'
					if (associated(p%next))then
						write(*,'(A)',advance='no')','
					else
						write(*,'(A)')''
					end if
					p=>p%next
				end do
			end if
		end do
	end if

end subroutine

subroutine print(U,f_unit)

	class(unidic),intent(inout)::U
	integer,intent(in)::f_unit
	type(node),pointer::p
	integer::i

	write(f_unit,*)U%item_num
	write(f_unit,*)U%stack_num
	write(f_unit,*)U%avail_stack(:U%stack_num)
	do i=1,hash_size
		if(associated(U%hash_ary(i)%first))then
			p=>U%hash_ary(i)%first
			do while(associated(p))
				write(f_unit,*)trim(p%key),p%val
				p=>p%next
			end do
		end if
	end do

end subroutine

subroutine read(U,f_unit)

	class(unidic),intent(inout)::U
	integer,intent(in)::f_unit
	type(node),pointer::p
	integer::i,val
	character(len=max_char_length)::key

	read(f_unit,*)U%item_num
	read(f_unit,*)U%stack_num
	read(f_unit,*)U%avail_stack(:U%stack_num)
	do i=1,U%item_num
		read(f_unit,*)key,val
		call U%add_with_val(key,val)
	end do

end subroutine

subroutine add(U,key,val)

	class(unidic),intent(inout)::U
	character(len=*),intent(in)::key
	integer,intent(out)::val

	U%item_num=U%item_num+1

	if(U%stack_num==0)then
		val=U%item_num
	else
		val=U%avail_stack(U%stack_num)
		U%stack_num=U%stack_num-1
	end if

	call U%add_with_val(key,val)

end subroutine

subroutine add_with_val(U,key,val)

	class(unidic),intent(inout)::U
	character(len=*),intent(in)::key
	integer,intent(in)::val
	type(node),pointer::p
	integer::pos

	pos=hash_func(U,key)

	if(.not.associated(U%hash_ary(pos)%first))then
		allocate(U%hash_ary(pos)%first)
		U%hash_ary(pos)%first%key=key
		U%hash_ary(pos)%first%val=val
	else
		p=>U%hash_ary(pos)%first
		if(p%key==key)then
			call wc_error_stop('unidic.add','key '//trim(key)//' already exist')
		end if
		do while(associated(p%next))
			p=>p%next
			if(p%key==key)then
				call wc_error_stop('unidic.add','key '//trim(key)//' already exist')
			end if
		end do
		allocate(p%next)
		p%next%key=key
		p%next%val=val
	end if

end subroutine

subroutine del_with_val(U,key,val)

	class(unidic),intent(inout)::U
	character(len=*),intent(in)::key
	integer,intent(out)::val
	type(node),pointer::p,q
	integer::pos
	logical::found

	pos=hash_func(U,key)

	found=.false.
	if(associated(U%hash_ary(pos)%first))then
		p=>U%hash_ary(pos)%first			!first node
		if(p%key==key)then
			val=p%val
			U%hash_ary(pos)%first=>p%next
			deallocate(p)
			found=.true.
		else
			do while(associated(p%next))
				if(p%next%key==key)then
					val=p%next%val
					q=>p%next
					p%next=>p%next%next
					deallocate(q)
					found=.true.
					exit
				end if
				p=>p%next
			end do
		end if
	end if

	if(.not.found) call wc_error_stop('unidic.del','key '//trim(key)//' not exist')

end subroutine

subroutine del(U,key,val_)

	class(unidic),intent(inout)::U
	character(len=*),intent(in)::key
	type(node),pointer::p,q
	integer::val

	call U%del_with_val(key,val)
	U%item_num=U%item_num-1
	U%stack_num=U%stack_num+1
	U%avail_stack(U%stack_num)=val

end subroutine

integer function val(U,key)

	class(unidic),intent(in)::U
	character(len=*),intent(in)::key
	type(node),pointer::p
	integer::pos
	logical::found

	pos=hash_func(U,key)

	found=.false.
	if(associated(U%hash_ary(pos)%first))then
		p=>U%hash_ary(pos)%first			!first node
		do while(associated(p))
			if(p%key==key)then
				val=p%val
				found=.true.
				exit
			end if
			p=>p%next
		end do
	end if

	if(.not.found) val=0

end function

subroutine rename(U,key1,key2)

	class(unidic),intent(inout)::U
	character(len=*),intent(in)::key1,key2
	integer::val

	call U%del_with_val(key1,val)
	call U%add_with_val(key2,val)

end subroutine

integer function hash_func(U,key)

	class(unidic),intent(in)::U
	character(len=*),intent(in)::key
	integer::i

	hash_func=0
	do i=1,len_trim(key)
		hash_func=hash_func+iachar(key(i:i))
	end do
	hash_func=mod(hash_func,hash_size)+1
		
end function

end module type_unidic