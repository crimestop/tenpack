module type_unidic
!! (in libkernel)
!! the module to return a unidic positive int for each string
use error
use string
use mod_stack
implicit none
private

type node
	private
	type(node),pointer::next=>null()
	character(len=max_char_length)::key=''
	integer::val=0
end type

integer,parameter::hash_size=787

type node_head
	private
	type(node),pointer::first=>null()
end type

type iterate_state
	private
	type(node),pointer::pos=>null()
	integer::head_pos=0
	logical::iterate_tag=.false.
end type

type unidic
	!! the class to hold key-index pairs, keys are strings, indices are unique ints
	private
	type(node_head)::hash_ary(hash_size)
	integer::item_num=0
	type(stack)::avail_stack
	type(iterate_state)::state
	contains
	private
	procedure,public::num
	!! the number of items in a unidic
	procedure,public::add
	!! add a key to a unidic, return its index
	procedure::add_with_val
	procedure,public::del
	!! delete a key in a unidic, return its index
	procedure::del_with_val
	procedure,public::val
	!! find a key in a unidic, return its index
	procedure,public::show
	!! show a unidic
	procedure,public::rename
	!! rename a key in a unidic
	procedure,public:: clean
	!! clean a unidic
	procedure,public:: print
	!! print a unidic to a file
	procedure,public:: read
	!! read a unidic from a file
	procedure,public:: iterate
	!! iterate a unidic.
	!! Use it in this way: do while(U%iterate(key,val))
	procedure::copy
	generic,public :: assignment(=) => copy
	!! assignment of a unidic
	!procedure,public::check_consistency
	final:: clean_dic
	!! clean the object to avoid memory leak

end type unidic

public unidic
contains

function num(U) result(res)

	class(unidic),intent(in)::U
	integer :: res

	res = U%item_num

end function

subroutine clean_dic(U)

	type(unidic),intent(inout)::U

	call U%clean()

end subroutine

subroutine clean(U)

	class(unidic),intent(inout)::U
	type(node),pointer::p,p_n
	integer::i

	if(U%state%iterate_tag) call wc_error_stop('unidic.rename','in iteration')
	U%item_num=0
	call U%avail_stack%clean()
	U%state%pos=>null()
	U%state%head_pos=0
	U%state%iterate_tag=.false.
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
	U_out%avail_stack=U_in%avail_stack

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
		call write_message('Empty dictionary.')
	else
		call write_message('Dictionary with '//trim(str(U%item_num))//' items:')
		do i=1,hash_size
			if(associated(U%hash_ary(i)%first))then
				p=>U%hash_ary(i)%first
				call write_message('  hash('//trim(str(i))//'):','no')
				do while(associated(p))
					call write_message(' ('//trim(p%key)//':'//trim(str(p%val))//')','no')
					if (associated(p%next))then
						call write_message(',','no')
					else
						call write_message('')
					end if
					p=>p%next
				end do
			end if
		end do
		if(U%state%iterate_tag) then
			call write_message(' In iteration mode')
		else
			call write_message(' Not in iteration mode')
		end if
	end if

end subroutine

subroutine print(U,f_unit)

	class(unidic),intent(inout)::U
	integer,intent(in)::f_unit
	type(node),pointer::p
	integer::i

	write(f_unit,*)U%item_num
	call U%avail_stack%write(f_unit)
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

	call clean(U)
	read(f_unit,*)U%item_num
	call U%avail_stack%read(f_unit)
	do i=1,U%item_num
		read(f_unit,*)key,val
		call U%add_with_val(key,val)
	end do

end subroutine

subroutine add(U,key,val)

	class(unidic),intent(inout)::U
	character(len=*),intent(in)::key
	integer,intent(out)::val

	if(U%state%iterate_tag) call wc_error_stop('unidic.add','in iteration')

	U%item_num=U%item_num+1
	if(U%avail_stack%num()==0)then
		val=U%item_num
	else
		val=U%avail_stack%pop()
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

subroutine del(U,key)

	class(unidic),intent(inout)::U
	character(len=*),intent(in)::key
	type(node),pointer::p,q
	integer::val

	if(U%state%iterate_tag) call wc_error_stop('unidic.del','in iteration')
	call U%del_with_val(key,val)
	U%item_num=U%item_num-1
	call U%avail_stack%push(val)

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

	if(U%state%iterate_tag) call wc_error_stop('unidic.rename','in iteration')
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

function iterate(U,key,val) result(res)

	class(unidic),intent(inout)::U
	character(len=max_char_length),intent(out)::key
	integer,intent(out)::val
	logical::res
	type(node),pointer::p

	key=''
	val=0
	res=.false.
	if (.not. (U%state%iterate_tag))then
		U%state%head_pos=0
		do while(U%state%head_pos<hash_size)
			U%state%head_pos=U%state%head_pos+1
			if(associated(U%hash_ary(U%state%head_pos)%first))then
				U%state%pos=>U%hash_ary(U%state%head_pos)%first
				key=U%state%pos%key
				val=U%state%pos%val
				res=.true.
				exit
			end if
		end do
	else if (associated(U%state%pos%next))then
		U%state%pos=>U%state%pos%next
		key=U%state%pos%key
		val=U%state%pos%val
		res=.true.
	else if(U%state%head_pos<hash_size) then
		do while(U%state%head_pos<hash_size)
			U%state%head_pos=U%state%head_pos+1
			if(associated(U%hash_ary(U%state%head_pos)%first))then
				U%state%pos=>U%hash_ary(U%state%head_pos)%first
				key=U%state%pos%key
				val=U%state%pos%val
				res=.true.
				exit
			end if
		end do
	end if
	U%state%iterate_tag=res

end function

end module type_unidic
