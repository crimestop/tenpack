module mod_dictionary
use error
use string
use tensor_type
implicit none
private

integer,parameter::itemmaxn=100
type dictionary

	private
	integer,allocatable::int_val(:)
	character(len=max_char_length),allocatable:: int_name(:)
	real(8),allocatable::real_val(:)
	character(len=max_char_length),allocatable :: real_name(:)
	complex(8),allocatable::com_val(:)
	character(len=max_char_length),allocatable :: com_name(:)
	character(len=max_char_length),allocatable :: char_val(:)
	character(len=max_char_length),allocatable :: char_name(:)
	logical,allocatable :: logi_val(:)
	character(len=max_char_length),allocatable :: logi_name(:)
	type(tensor),allocatable :: ten_val(:)
	character(len=max_char_length),allocatable :: ten_name(:)
	integer::itemcurn(6)=0
	logical::inited=.false.

contains

	private
	procedure::init
	procedure,public::read
	procedure,public::print
	procedure,public::get_names
	procedure,public::sub_name
	procedure,public::sub_val
	procedure,public::clean
	procedure,public::rename
	procedure,public::setvalue
	procedure,public::getvalue
	procedure,public::insert
	procedure,public::delete
	procedure,public::ii,di,zi,ai,li,ti
	procedure,public::append
	procedure::find

end type

public dictionary

contains

subroutine init(D)
implicit none

	class(dictionary), intent(inout)::D

	allocate(D%int_val(itemmaxn))
	allocate(D%int_name(itemmaxn))
	allocate(D%real_val(itemmaxn))
	allocate(D%real_name(itemmaxn))
	allocate(D%com_val(itemmaxn))
	allocate(D%com_name(itemmaxn))
	allocate(D%char_val(itemmaxn))
	allocate(D%char_name(itemmaxn))
	allocate(D%logi_val(itemmaxn))
	allocate(D%logi_name(itemmaxn))
	allocate(D%ten_val(itemmaxn))
	allocate(D%ten_name(itemmaxn))
	D%inited=.true.
	call D%clean()

end subroutine

subroutine clean(D)
implicit none

	class(dictionary), intent(inout)::D
	integer::i

	if (.not. D%inited) call D%init()
	D%int_val=0
	D%int_name=''
	D%real_val=0
	D%real_name=''
	D%com_val=0
	D%com_name=''
	D%char_val=''
	D%char_name=''
	D%logi_val=.false.
	D%logi_name=''
	do i=1,D%itemcurn(6)
		call D%ten_val(i)%deallocate()
	end do
	D%ten_name=''
	D%itemcurn=0

end subroutine

function get_names(D) result(res)
implicit none

	class(dictionary), intent(in)::D
	character(len=max_char_length),allocatable::res(:)
	integer::num

	allocate(res(sum(D%itemcurn)))

	num=0
	res(num+1:num+D%itemcurn(1))=D%int_name(:D%itemcurn(1))
	num=num+D%itemcurn(1)
	res(num+1:num+D%itemcurn(2))=D%real_name(:D%itemcurn(2))
	num=num+D%itemcurn(2)
	res(num+1:num+D%itemcurn(3))=D%com_name(:D%itemcurn(3))
	num=num+D%itemcurn(3)
	res(num+1:num+D%itemcurn(4))=D%char_name(:D%itemcurn(4))
	num=num+D%itemcurn(4)
	res(num+1:num+D%itemcurn(5))=D%logi_name(:D%itemcurn(5))
	num=num+D%itemcurn(5)
	res(num+1:num+D%itemcurn(6))=D%ten_name(:D%itemcurn(6))

end function

function sub_val(D,value) result(res)
implicit none

	class(dictionary), intent(in)::D
	type(dictionary)::res
	class(*),intent(in) :: value
	integer::i

	call res%init()
	if (.not. D%inited) return

	select type(value)
	type is(integer)
		do i=1,D%itemcurn(1)
			if(D%int_val(i)==value)then
				call res%insert(D%int_name(i),value)
			end if
		end do
	type is(real(8))
		do i=1,D%itemcurn(2)
			if(abs(D%real_val(i)-value) <= 1d-8*abs(value))then
				call res%insert(D%real_name(i),value)
			end if
		end do
	type is(complex(8))
		do i=1,D%itemcurn(3)
			if(abs(D%com_val(i)-value) <= 1d-8*abs(value))then
				call res%insert(D%com_name(i),value)
			end if
		end do
	type is(character(len=*))
		do i=1,D%itemcurn(4)
			if(D%char_val(i)==value)then
				call res%insert(D%char_name(i),value)
			end if
		end do
	type is(logical)
		do i=1,D%itemcurn(5)
			if(D%logi_val(i) .eqv. value)then
				call res%insert(D%logi_name(i),value)
			end if
		end do
	end select

end function

subroutine insert(D,name,value)
implicit none

	class(dictionary), intent(inout)::D
	character(len=*), intent(in) :: name
	class(*), intent(in) :: value
	integer::type

	if (.not. D%inited) call D%init()
	select type(value)
	type is(integer)
		type=1
		if(D%itemcurn(type)==itemmaxn)then
			call wc_error_stop('para-insert','num of integer parameter reaches limit')
		else
			D%itemcurn(type)=D%itemcurn(type)+1
			D%int_val(D%itemcurn(type))=value
			D%int_name(D%itemcurn(type))=name
		end if
	type is(real(8))
		type=2
		if(D%itemcurn(type)==itemmaxn)then
			call wc_error_stop('para-insert','num of real parameter reaches limit')
		else
			D%itemcurn(type)=D%itemcurn(type)+1
			D%real_val(D%itemcurn(type))=value
			D%real_name(D%itemcurn(type))=name
		end if
	type is(complex(8))
		type=3
		if(D%itemcurn(type)==itemmaxn)then
			call wc_error_stop('para-insert','num of complex parameter reaches limit')
		else
			D%itemcurn(type)=D%itemcurn(type)+1
			D%com_val(D%itemcurn(type))=value
			D%com_name(D%itemcurn(type))=name
		end if
	type is(character(len=*))
		type=4
		if(D%itemcurn(type)==itemmaxn)then
			call wc_error_stop('para-insert','num of character parameter reaches limit')
		else
			D%itemcurn(type)=D%itemcurn(type)+1
			D%char_val(D%itemcurn(type))=value
			D%char_name(D%itemcurn(type))=name
		end if
	type is (logical)
		type=5
		if(D%itemcurn(type)==itemmaxn)then
			call wc_error_stop('para-insert','num of logical parameter reaches limit')
		else
			D%itemcurn(type)=D%itemcurn(type)+1
			D%logi_val(D%itemcurn(type))=value
			D%logi_name(D%itemcurn(type))=name
		end if
	type is (tensor)
		type=6
		if(D%itemcurn(type)==itemmaxn)then
			call wc_error_stop('para-insert','num of logical parameter reaches limit')
		else
			D%itemcurn(type)=D%itemcurn(type)+1
			D%ten_val(D%itemcurn(type))=value
			D%ten_name(D%itemcurn(type))=name
		end if
	end select

end subroutine

integer function ii(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer::type,pos

	call D%find(name,type,pos)

	select case (type)
	case (1)
		res=D%int_val(pos)
	case (2)
		res=D%real_val(pos)
	case (3)
		res=D%com_val(pos)
	case (4)
		call wc_error_stop('para-ii','cannot change from char to int')
	case (5)
		call wc_error_stop('para-ii','cannot change from logical to int')
	case (6)
		call wc_error_stop('para-ii','cannot change from tensor to int')
	end select

end function

real(8) function di(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer::type,pos

	call D%find(name,type,pos)

	select case (type)
	case (1)
		res=D%int_val(pos)
	case (2)
		res=D%real_val(pos)
	case (3)
		res=D%com_val(pos)
	case (4)
		call wc_error_stop('para-di','cannot change from char to real')
	case (5)
		call wc_error_stop('para-di','cannot change from logical to real')
	case (6)
		call wc_error_stop('para-di','cannot change from tensor to real')
	end select

end function

complex(8) function zi(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer::type,pos

	call D%find(name,type,pos)

	select case (type)
	case (1)
		res=D%int_val(pos)
	case (2)
		res=D%real_val(pos)
	case (3)
		res=D%com_val(pos)
	case (4)
		call wc_error_stop('para-zi','cannot change from char to complex')
	case (5)
		call wc_error_stop('para-zi','cannot change from logical to complex')
	case (6)
		call wc_error_stop('para-zi','cannot change from tensor to complex')
	end select

end function

character(len=max_char_length) function ai(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer::type,pos

	call D%find(name,type,pos)

	select case (type)
	case (1)
		res=str(D%int_val(pos))
	case (2)
		res=str(D%real_val(pos))
	case (3)
		res=str(D%com_val(pos))
	case (4)
		res=trim(D%char_val(pos))
	case (5)
		res=str(D%logi_val(pos))
	case (6)
		call wc_error_stop('para-ai','cannot change from tensor to char')
	end select

end function

logical function li(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer::type,pos

	call D%find(name,type,pos)

	select case (type)
	case (1)
		call wc_error_stop('para-li','cannot change from int to logical')
	case (2)
		call wc_error_stop('para-li','cannot change from real to logical')
	case (3)
		call wc_error_stop('para-li','cannot change from complex to logical')
	case (4)
		call wc_error_stop('para-li','cannot change from char to logical')
	case (5)
		res=D%logi_val(pos)
	case (6)
		call wc_error_stop('para-li','cannot change from tensor to logical')
	end select

end function

type(tensor) function ti(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer::type,pos

	call D%find(name,type,pos)

	select case (type)
	case (1)
		call wc_error_stop('para-ti','cannot change from int to tensor')
	case (2)
		call wc_error_stop('para-ti','cannot change from real to tensor')
	case (3)
		call wc_error_stop('para-ti','cannot change from complex to tensor')
	case (4)
		call wc_error_stop('para-ti','cannot change from char to tensor')
	case (5)
		call wc_error_stop('para-ti','cannot change from logical to tensor')
	case (6)
		res=D%ten_val(pos)
	end select

end function

subroutine find(D,name,type,pos)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer,intent(out)::type,pos
	integer::i

	if (.not. D%inited) call wc_error_stop('dictionary.find','dictionary is uninitiated')

	do i=1,D%itemcurn(1)
		if(D%int_name(i) == name) then
			type=1
			pos=i
			return
		end if
	end do

	do i=1,D%itemcurn(2)
		if(D%real_name(i) == name) then
			type=2
			pos=i
			return
		end if
	end do

	do i=1,D%itemcurn(3)
		if(D%com_name(i) == name) then
			type=3
			pos=i
			return
		end if
	end do

	do i=1,D%itemcurn(4)
		if(D%char_name(i) == name) then
			type=4
			pos=i
			return
		end if
	end do

	do i=1,D%itemcurn(5)
		if(D%logi_name(i) == name) then
			type=5
			pos=i
			return
		end if
	end do

	do i=1,D%itemcurn(6)
		if(D%ten_name(i) == name) then
			type=6
			pos=i
			return
		end if
	end do

	call wc_error_stop('dictionary.find',trim(name)//' not found in the paralist.')

end subroutine

subroutine delete(D,name)
implicit none

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	integer::type,pos

	call D%find(name,type,pos)

	select case (type)
	case (1)
		D%int_val(pos:itemmaxn-1)=D%int_val(pos+1:itemmaxn)
		D%int_name(pos:itemmaxn-1)=D%int_name(pos+1:itemmaxn)
	case (2)
		D%real_val(pos:itemmaxn-1)=D%real_val(pos+1:itemmaxn)
		D%real_name(pos:itemmaxn-1)=D%real_name(pos+1:itemmaxn)
	case (3)
		D%com_val(pos:itemmaxn-1)=D%com_val(pos+1:itemmaxn)
		D%com_name(pos:itemmaxn-1)=D%com_name(pos+1:itemmaxn)
	case (4)
		D%char_val(pos:itemmaxn-1)=D%char_val(pos+1:itemmaxn)
		D%char_name(pos:itemmaxn-1)=D%char_name(pos+1:itemmaxn)
	case (5)
		D%logi_val(pos:itemmaxn-1)=D%logi_val(pos+1:itemmaxn)
		D%logi_name(pos:itemmaxn-1)=D%logi_name(pos+1:itemmaxn)
	case (6)
		D%ten_val(pos:itemmaxn-1)=D%ten_val(pos+1:itemmaxn)
		D%ten_name(pos:itemmaxn-1)=D%ten_name(pos+1:itemmaxn)
	end select

	D%itemcurn(type)=D%itemcurn(type)-1

end subroutine

subroutine setvalue(D,name,val)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	class(*),intent(in)::val
	integer::type,pos

	call D%find(name,type,pos)

	select type(val)
	type is (integer)
		select case (type)
		case (1)
			D%int_val(pos)=val
		case (2)
			D%real_val(pos)=val
		case (3)
			D%com_val(pos)=val
		case (4)
			D%char_val(pos)=val
		case (5)
			call wc_error_stop('para-setvalue','cannot change from int to logical')
		case (6)
			call wc_error_stop('para-setvalue','cannot change from int to tensor')
		end select
	type is(real(8))
		select case (type)
		case (1)
			D%int_val(pos)=val
		case (2)
			D%real_val(pos)=val
		case (3)
			D%com_val(pos)=val
		case (4)
			D%char_val(pos)=val
		case (5)
			call wc_error_stop('para-setvalue','cannot change from real to logical')
		case (6)
			call wc_error_stop('para-setvalue','cannot change from real to tensor')
		end select
	type is (complex(8))
		select case (type)
		case (1)
			D%int_val(pos)=val
		case (2)
			D%real_val(pos)=val
		case (3)
			D%com_val(pos)=val
		case (4)
			D%char_val(pos)=val
		case (5)
			call wc_error_stop('para-setvalue','cannot change from complex to logical')
		case (6)
			call wc_error_stop('para-setvalue','cannot change from complex to tensor')
		end select
	type is (character(len=*))
		select case (type)
		case (1)
			call wc_error_stop('para-setvalue','cannot change from char to int')
		case (2)
			call wc_error_stop('para-setvalue','cannot change from char to real')
		case (3)
			call wc_error_stop('para-setvalue','cannot change from char to complex')
		case (4)
			D%char_val(pos)=val
		case (5)
			call wc_error_stop('para-setvalue','cannot change from char to logical')
		case (6)
			call wc_error_stop('para-setvalue','cannot change from char to tensor')
		end select
	type is (logical)
		select case (type)
		case (1)
			call wc_error_stop('para-setvalue','cannot change from logical to int')
		case (2)
			call wc_error_stop('para-setvalue','cannot change from logical to real')
		case (3)
			call wc_error_stop('para-setvalue','cannot change from logical to complex')
		case (4)
			D%char_val(pos)=val
		case (5)
			D%logi_val(pos)=val
		case (6)
			call wc_error_stop('para-setvalue','cannot change from logical to tensor')
		end select
	type is (tensor)
		select case (type)
		case (1)
			call wc_error_stop('para-setvalue','cannot change from tensor to int')
		case (2)
			call wc_error_stop('para-setvalue','cannot change from tensor to real')
		case (3)
			call wc_error_stop('para-setvalue','cannot change from tensor to complex')
		case (4)
			call wc_error_stop('para-setvalue','cannot change from tensor to char')
		case (5)
			call wc_error_stop('para-setvalue','cannot change from tensor to logical')
		case (6)
			D%ten_val(pos)=val
		end select
	end select

end subroutine

subroutine getvalue(D,name,val)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	class(*),intent(inout)::val
	integer::type,pos

	call D%find(name,type,pos)

	select type(val)
	type is (integer)
		select case (type)
		case (1)
			val=D%int_val(pos)
		case (2)
			val=D%real_val(pos)
		case (3)
			val=D%com_val(pos)
		case (4)
			call wc_error_stop('para-setvalue','cannot change from char to int')
		case (5)
			call wc_error_stop('para-setvalue','cannot change from logical to int')
		case (6)
			call wc_error_stop('para-setvalue','cannot change from tensor to int')
		end select
	type is (real(8))
		select case (type)
		case (1)
			val=D%int_val(pos)
		case (2)
			val=D%real_val(pos)
		case (3)
			val=D%com_val(pos)
		case (4)
			call wc_error_stop('para-setvalue','cannot change from char to real')
		case (5)
			call wc_error_stop('para-setvalue','cannot change from logical to real')
		case (6)
			call wc_error_stop('para-setvalue','cannot change from tensor to real')
		end select
	type is (complex(8))
		select case (type)
		case (1)
			val=D%int_val(pos)
		case (2)
			val=D%real_val(pos)
		case (3)
			val=D%com_val(pos)
		case (4)
			call wc_error_stop('para-setvalue','cannot change from char to complex')
		case (5)
			call wc_error_stop('para-setvalue','cannot change from logical to complex')
		case (6)
			call wc_error_stop('para-setvalue','cannot change from tensor to complex')
		end select
	type is (character(len=*))
		select case (type)
		case (1)
			call wc_error_stop('para-setvalue','cannot change from int to char')
		case (2)
			call wc_error_stop('para-setvalue','cannot change from real to char')
		case (3)
			call wc_error_stop('para-setvalue','cannot change from complex to char')
		case (4)
			val=D%char_val(pos)
		case (5)
			call wc_error_stop('para-setvalue','cannot change from logical to char')
		case (6)
			call wc_error_stop('para-setvalue','cannot change from tensor to char')
		end select
	type is (logical)
		select case (type)
		case (1)
			call wc_error_stop('para-setvalue','cannot change from int to logical')
		case (2)
			call wc_error_stop('para-setvalue','cannot change from real to logical')
		case (3)
			call wc_error_stop('para-setvalue','cannot change from complex to logical')
		case (4)
			call wc_error_stop('para-setvalue','cannot change from character to logical')
		case (5)
			val=D%logi_val(pos)
		case (6)
			call wc_error_stop('para-setvalue','cannot change from tensor to logical')
		end select
	type is (tensor)
		select case (type)
		case (1)
			call wc_error_stop('para-setvalue','cannot change from int to tensor')
		case (2)
			call wc_error_stop('para-setvalue','cannot change from real to tensor')
		case (3)
			call wc_error_stop('para-setvalue','cannot change from complex to tensor')
		case (4)
			call wc_error_stop('para-setvalue','cannot change from char to tensor')
		case (5)
			call wc_error_stop('para-setvalue','cannot change from  logical to tensor')
		case (6)
			val=D%ten_val(pos)
		end select
	end select

end subroutine


subroutine rename(D,name,new_name)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	character(len=*),intent(inout)::new_name
	integer::type,pos

	call D%find(name,type,pos)

	select case (type)
	case (1)
		D%int_name(pos)=new_name
	case (2)
		D%real_name(pos)=new_name
	case (3)
		D%com_name(pos)=new_name
	case (4)
		D%char_name(pos)=new_name
	case (5)
		D%logi_name(pos)=new_name
	case (6)
		D%ten_name(pos)=new_name
	end select

end subroutine

type(dictionary) function sub_name(D,names) result(out_list)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::names(:)
	integer::i
	integer::type,pos

	do i=1,size(names)
		call D%find(names(i),type,pos)
		select case (type)
		case (1)
			call out_list%insert(names(i),D%int_val(pos))
		case (2)
			call out_list%insert(names(i),D%real_val(pos))
		case (3)
			call out_list%insert(names(i),D%com_val(pos))
		case (4)
			call out_list%insert(names(i),D%char_val(pos))
		case (5)
			call out_list%insert(names(i),D%logi_val(pos))
		case (6)
			call out_list%insert(names(i),D%ten_val(pos))
		end select
	end do

end function

subroutine print(D,unit,end_tag_)

	class(dictionary),intent(in)::D
	integer,intent(in),optional::unit
	logical,intent(in),optional::end_tag_
	logical::end_tag
	integer::k
	character(len=max_char_length+20):: formatl

	end_tag=.true.
	if(present(end_tag_)) end_tag=end_tag_

	if (.not. D%inited) then
		if(present(unit) .and. end_tag) write(unit,*) '/'
		return 
	end if

	do k=1,D%itemcurn(1)
		formatl='i'
		formatl(3:)=D%int_name(k)
		formatl(20:)=str(D%int_val(k))
		if(present(unit))then
			write(unit,*) trim(formatl)
		else
			call writemess(formatl)
		end if
	end do

	do k=1,D%itemcurn(2)
		formatl='d'
		formatl(3:)=D%real_name(k)
		formatl(20:)=str(D%real_val(k))
		if(present(unit))then
			write(unit,*) trim(formatl)
		else
			call writemess(formatl)
		end if
	end do

	do k=1,D%itemcurn(3)
		formatl='z'
		formatl(3:)=D%com_name(k)
		formatl(20:)=str(D%com_val(k))
		if(present(unit))then
			write(unit,*) trim(formatl)
		else
			call writemess(formatl)
		end if
	end do

	do k=1,D%itemcurn(4)
		formatl='a'
		formatl(3:)=D%char_name(k)
		formatl(20:)=D%char_val(k)
		if(present(unit))then
			write(unit,*) trim(formatl)
		else
			call writemess(formatl)
		end if
	end do

	do k=1,D%itemcurn(5)
		formatl='l'
		formatl(3:)=D%logi_name(k)
		formatl(20:)=str(D%logi_val(k))
		if(present(unit))then
			write(unit,*) trim(formatl)
		else
			call writemess(formatl)
		end if
	end do	

	do k=1,D%itemcurn(6)
		formatl='t'
		formatl(3:)=D%ten_name(k)
		formatl(20:)='printed below'
		if(present(unit))then
			write(unit,*) trim(formatl)
			call D%ten_val(k)%write(unit)
		else
			call writemess(formatl)
			call D%ten_val(k)%print()
		end if
	end do	

	if(present(unit) .and. end_tag) write(unit,*) '/'

end subroutine

subroutine read(G,unit)

	class(dictionary),intent(inout)::G
	integer,intent(in)::unit
	character(len=max_char_length)::cur_class
	character(len=10*max_char_length)::line
	integer::io_stat

	character(len=max_char_length)::no_use,cur_name
	integer::ival 
	real(8)::dval 
	complex(8)::zval 
	character(len=max_char_length)::aval 
	logical::lval 
	type(tensor)::tval

	if (.not. G%inited) call G%init()
	call G%clean()

	read(unit,'(A)',IOSTAT=io_stat) line
	do while(.true.)
		if(io_stat/=0)then
			call wc_error_stop('dictionary.read','EOF before identifier "/"')
		end if
		line=adjustl(line)
		select case(line(1:1))
		case('i')
			read(line(2:),*) cur_name,ival
			call G%insert(cur_name,ival)
		case('d')
			read(line(2:),*) cur_name,dval
			call G%insert(cur_name,dval)
		case('z')
			read(line(2:),*) cur_name,zval
			call G%insert(cur_name,zval)
		case('a')
			read(line(2:),*) cur_name,aval
			call G%insert(cur_name,aval)
		case('l')
			read(line(2:),*) cur_name,lval
			call G%insert(cur_name,lval)
		case('t')
			read(line(2:),*) cur_name
			call tval%read(unit)
			call G%insert(cur_name,tval)
		case('/')
			exit
		end select

		read(unit,'(A)',IOSTAT=io_stat) line
	end do

end subroutine

subroutine append(P1,P2)

	class(dictionary),intent(inout)::P1
	type(dictionary),intent(in)::P2
	integer::k

	if (.not. P1%inited) call P1%init()
	do k=1,P2%itemcurn(1)
		call P1%insert(P2%int_name(k),P2%int_val(k))
	end do

	do k=1,P2%itemcurn(2)
		call P1%insert(P2%real_name(k),P2%real_val(k))
	end do

	do k=1,P2%itemcurn(3)
		call P1%insert(P2%com_name(k),P2%com_val(k))
	end do

	do k=1,P2%itemcurn(4)
		call P1%insert(P2%char_name(k),P2%char_val(k))
	end do

	do k=1,P2%itemcurn(5)
		call P1%insert(P2%logi_name(k),P2%logi_val(k))
	end do	

	do k=1,P2%itemcurn(6)
		call P1%insert(P2%ten_name(k),P2%ten_val(k))
	end do	

end subroutine

end module 