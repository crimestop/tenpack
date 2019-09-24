module mod_dictionary
use error
use string
use tensor_type
implicit none
private

! type: int=1 dbl=2 cmplx=3 char=4 logi=5 ten=6 int_ary=7 dbl_ary=8 char_ary=9 logi_ary=10

integer,parameter::itemmaxn=100
integer,parameter::lenmax=10
integer,parameter::type_num=10
character(len=10),parameter:: type_name(type_num)=['integer   ','double    ','complex   ',&
	'character ','logical   ','tensor    ','int_ary   ','dbl_ary   ','char_ary  ','logi_ary  ']
character(len=2),parameter:: type_abbr(type_num)=['i ','d ','z ','a ','l ','t ','mi','md','ma','ml']
type dictionary

	private
	integer,allocatable::int_val(:)
	integer,allocatable::int_ary_val(:,:)
	integer,allocatable::int_ary_len(:)

	real(8),allocatable::double_val(:)
	real(8),allocatable::dbl_ary_val(:,:)
	integer,allocatable::dbl_ary_len(:)

	complex(8),allocatable::com_val(:)

	character(len=max_char_length),allocatable :: char_val(:)
	character(len=max_char_length),allocatable :: char_ary_val(:,:)
	integer,allocatable::char_ary_len(:)

	logical,allocatable :: logi_val(:)
	logical,allocatable :: logi_ary_val(:,:)
	integer,allocatable::logi_ary_len(:)

	type(tensor),allocatable :: ten_val(:)

	character(len=max_char_length),allocatable:: names(:,:)
	integer,allocatable::itemcurn(:)
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
	procedure::isetvalue,dsetvalue,zsetvalue,asetvalue,lsetvalue,tsetvalue,&
		misetvalue,mdsetvalue,masetvalue,mlsetvalue
	generic,public::setvalue=>isetvalue,dsetvalue,zsetvalue,asetvalue,lsetvalue,tsetvalue,&
		misetvalue,mdsetvalue,masetvalue,mlsetvalue
	procedure::igetvalue,dgetvalue,zgetvalue,agetvalue,lgetvalue,tgetvalue,&
		migetvalue,mdgetvalue,magetvalue,mlgetvalue
	generic,public::getvalue=>igetvalue,dgetvalue,zgetvalue,agetvalue,lgetvalue,tgetvalue,&
		migetvalue,mdgetvalue,magetvalue,mlgetvalue
	procedure::insert_int,insert_dbl,insert_com,insert_char,insert_logi,insert_ten,&
		insert_int_ary,insert_dbl_ary,insert_char_ary,insert_logi_ary
	generic,public::insert=>insert_int,insert_dbl,insert_com,insert_char,insert_logi,insert_ten,&
		insert_int_ary,insert_dbl_ary,insert_char_ary,insert_logi_ary
	procedure,public::delete
	procedure,public::ii,di,zi,ai,li,ti,mii,mdi,mai,mli
	procedure,public::append
	procedure,public::check_contain
	procedure::find
	procedure,public::search
	procedure::append_by_name

end type

public dictionary

contains

subroutine init(D)
implicit none

	class(dictionary), intent(inout)::D

	allocate(D%int_val(itemmaxn))
	allocate(D%double_val(itemmaxn))
	allocate(D%com_val(itemmaxn))
	allocate(D%char_val(itemmaxn))
	allocate(D%logi_val(itemmaxn))
	allocate(D%ten_val(itemmaxn))
	allocate(D%int_ary_val(lenmax,itemmaxn))
	allocate(D%int_ary_len(itemmaxn))
	allocate(D%dbl_ary_val(lenmax,itemmaxn))
	allocate(D%dbl_ary_len(itemmaxn))
	allocate(D%char_ary_val(lenmax,itemmaxn))
	allocate(D%char_ary_len(itemmaxn))
	allocate(D%logi_ary_val(lenmax,itemmaxn))
	allocate(D%logi_ary_len(itemmaxn))
	allocate(D%names(itemmaxn,type_num))
	allocate(D%itemcurn(type_num))
	D%inited=.true.
	call D%clean()

end subroutine

subroutine clean(D)

	class(dictionary), intent(inout)::D
	integer::i

	if (.not. D%inited) call D%init()
	D%names=''
	D%itemcurn=0
	D%int_val=0
	D%double_val=0d0
	D%com_val=0d0
	D%char_val=''
	D%logi_val=.false.
	do i=1,D%itemcurn(6)
		call D%ten_val(i)%deallocate()
	end do
	D%int_ary_val=0
	D%int_ary_len=0
	D%dbl_ary_val=0d0
	D%dbl_ary_len=0
	D%char_ary_val=''
	D%char_ary_len=0
	D%logi_ary_val=.false.
	D%logi_ary_len=0


end subroutine

function get_names(D) result(res)

	class(dictionary), intent(in)::D
	character(len=max_char_length),allocatable::res(:)
	integer::num,type

	allocate(res(sum(D%itemcurn)))
	num=0
	do type=1,type_num
		if(D%itemcurn(type)>0) then
			res(num+1:num+D%itemcurn(type))=D%names(:D%itemcurn(type),type)
			num=num+D%itemcurn(type)
		end if
	end do

end function

function sub_val(D,value) result(res)

	class(dictionary), intent(in)::D
	type(dictionary)::res
	character(len=*), intent(in) :: value
	integer::i

	call res%init()
	if (.not. D%inited) return
	do i=1,D%itemcurn(4)
		if(D%char_val(i)==value)then
			call res%insert(D%names(i,4),value)
		end if
	end do

end function

subroutine insert_int(D,name,value)

	class(dictionary), intent(inout)::D
	character(len=*), intent(in) :: name
	integer, intent(in) :: value
	integer::type

	if (.not. D%inited) call D%init()
	type=1
	if(D%itemcurn(type)==itemmaxn)then
		call wc_error_stop('para-insert','num of integer parameter reaches limit')
	else
		D%itemcurn(type)=D%itemcurn(type)+1
		D%int_val(D%itemcurn(type))=value
		D%names(D%itemcurn(type),type)=name
	end if

end subroutine

subroutine insert_int_ary(D,name,value)

	class(dictionary), intent(inout)::D
	character(len=*), intent(in) :: name
	integer, intent(in) :: value(:)
	integer::type

	if (.not. D%inited) call D%init()
	type=7
	if(D%itemcurn(type)==itemmaxn)then
		call wc_error_stop('para-insert','num of integer_ary parameter reaches limit')
	else if (size(value)>lenmax) then
		call wc_error_stop('para-insert','length of integer_ary parameter reaches limit')
	else
		D%itemcurn(type)=D%itemcurn(type)+1
		D%int_ary_val(:size(value),D%itemcurn(type))=value
		D%int_ary_len(D%itemcurn(type))=size(value)
		D%names(D%itemcurn(type),type)=name
	end if

end subroutine

subroutine insert_dbl(D,name,value)

	class(dictionary), intent(inout)::D
	character(len=*), intent(in) :: name
	real(8), intent(in) :: value
	integer::type

	if (.not. D%inited) call D%init()
	type=2
	if(D%itemcurn(type)==itemmaxn)then
		call wc_error_stop('para-insert','num of real parameter reaches limit')
	else
		D%itemcurn(type)=D%itemcurn(type)+1
		D%double_val(D%itemcurn(type))=value
		D%names(D%itemcurn(type),type)=name
	end if

end subroutine

subroutine insert_dbl_ary(D,name,value)

	class(dictionary), intent(inout)::D
	character(len=*), intent(in) :: name
	real(8), intent(in) :: value(:)
	integer::type

	if (.not. D%inited) call D%init()
	type=8
	if(D%itemcurn(type)==itemmaxn)then
		call wc_error_stop('para-insert','num of dbl_ary parameter reaches limit')
	else if (size(value)>lenmax) then
		call wc_error_stop('para-insert','length of dbl_ary parameter reaches limit')
	else
		D%itemcurn(type)=D%itemcurn(type)+1
		D%dbl_ary_val(:size(value),D%itemcurn(type))=value
		D%dbl_ary_len(D%itemcurn(type))=size(value)
		D%names(D%itemcurn(type),type)=name
	end if

end subroutine

subroutine insert_com(D,name,value)

	class(dictionary), intent(inout)::D
	character(len=*), intent(in) :: name
	complex(8), intent(in) :: value
	integer::type

	if (.not. D%inited) call D%init()
	type=3
	if(D%itemcurn(type)==itemmaxn)then
		call wc_error_stop('para-insert','num of complex parameter reaches limit')
	else
		D%itemcurn(type)=D%itemcurn(type)+1
		D%com_val(D%itemcurn(type))=value
		D%names(D%itemcurn(type),type)=name
	end if

end subroutine

subroutine insert_char(D,name,value)

	class(dictionary), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in) :: value
	integer::type

	if (.not. D%inited) call D%init()
	type=4
	if(D%itemcurn(type)==itemmaxn)then
		call wc_error_stop('para-insert','num of character parameter reaches limit')
	else
		D%itemcurn(type)=D%itemcurn(type)+1
		D%char_val(D%itemcurn(type))=value
		D%names(D%itemcurn(type),type)=name
	end if

end subroutine

subroutine insert_char_ary(D,name,value)

	class(dictionary), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in) :: value(:)
	integer::type

	if (.not. D%inited) call D%init()
	type=9
	if(D%itemcurn(type)==itemmaxn)then
		call wc_error_stop('para-insert','num of character parameter reaches limit')
	else if (size(value)>lenmax) then
		call wc_error_stop('para-insert','length of character_ary parameter reaches limit')
	else
		D%itemcurn(type)=D%itemcurn(type)+1
		D%char_ary_val(:size(value),D%itemcurn(type))=value
		D%char_ary_len(D%itemcurn(type))=size(value)
		D%names(D%itemcurn(type),type)=name
	end if

end subroutine

subroutine insert_logi(D,name,value)

	class(dictionary), intent(inout)::D
	character(len=*), intent(in) :: name
	logical, intent(in) :: value
	integer::type
	
	if (.not. D%inited) call D%init()
	type=5
	if(D%itemcurn(type)==itemmaxn)then
		call wc_error_stop('para-insert','num of logical parameter reaches limit')
	else
		D%itemcurn(type)=D%itemcurn(type)+1
		D%logi_val(D%itemcurn(type))=value
		D%names(D%itemcurn(type),type)=name
	end if

end subroutine

subroutine insert_logi_ary(D,name,value)

	class(dictionary), intent(inout)::D
	character(len=*), intent(in) :: name
	logical, intent(in) :: value(:)
	integer::type
	
	if (.not. D%inited) call D%init()
	type=10
	if(D%itemcurn(type)==itemmaxn)then
		call wc_error_stop('para-insert','num of logical_ary parameter reaches limit')
	else if (size(value)>lenmax) then
		call wc_error_stop('para-insert','length of logical_ary parameter reaches limit')
	else
		D%itemcurn(type)=D%itemcurn(type)+1
		D%logi_ary_val(:size(value),D%itemcurn(type))=value
		D%logi_ary_len(D%itemcurn(type))=size(value)
		D%names(D%itemcurn(type),type)=name
	end if

end subroutine

subroutine insert_ten(D,name,value)

	class(dictionary), intent(inout)::D
	character(len=*), intent(in) :: name
	type(tensor), intent(in) :: value
	integer::type
	
	if (.not. D%inited) call D%init()
	type=6
	if(D%itemcurn(type)==itemmaxn)then
		call wc_error_stop('para-insert','num of logical parameter reaches limit')
	else
		D%itemcurn(type)=D%itemcurn(type)+1
		D%ten_val(D%itemcurn(type))=value
		D%names(D%itemcurn(type),type)=name
	end if

end subroutine

function ii(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer::res

	call D%igetvalue(name,res)

end function

function mii(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer,allocatable::res(:)

	call D%migetvalue(name,res)

end function

function di(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	real(8)::res

	call D%dgetvalue(name,res)

end function

function mdi(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	real(8),allocatable::res(:)

	call D%mdgetvalue(name,res)

end function

function zi(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	complex(8)::res

	call D%zgetvalue(name,res)

end function

function ai(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	character(len=max_char_length)::res

	call D%agetvalue(name,res)

end function

function mai(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	character(len=max_char_length),allocatable::res(:)

	call D%magetvalue(name,res)

end function

function li(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	logical::res

	call D%lgetvalue(name,res)

end function

function mli(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	logical,allocatable::res(:)

	call D%mlgetvalue(name,res)

end function

function ti(D,name) result(res)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	type(tensor)::res

	call D%tgetvalue(name,res)

end function


subroutine search(D,name,type,pos,existed)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer,intent(out)::type,pos
	logical,intent(out)::existed
	integer::i

	type=-1
	pos=-1
	existed=.false.

	if (D%inited) then
		do type=1,type_num
			do i=1,D%itemcurn(type)
				if(D%names(i,type) == name) then
					pos=i
					existed=.true.
				end if
			end do
			if(existed) exit
		end do
	end if

end subroutine

subroutine find(D,name,type,pos)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer,intent(out)::type,pos
	logical::existed
	integer::i

	call search(D,name,type,pos,existed)

	if (.not. existed) call wc_error_stop('dictionary.find',trim(name)//' not found in the paralist.')

end subroutine

function check_contain(D,name) result(res)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	logical::res
	integer::type,pos

	call D%search(name,type,pos,res)

end function

subroutine delete(D,name)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	integer::type,pos

	call D%find(name,type,pos)

	select case (type_name(type))
	case ('integer')
		D%int_val(pos:itemmaxn-1)=D%int_val(pos+1:itemmaxn)
	case ('double')
		D%double_val(pos:itemmaxn-1)=D%double_val(pos+1:itemmaxn)
	case ('complex')
		D%com_val(pos:itemmaxn-1)=D%com_val(pos+1:itemmaxn)
	case ('character')
		D%char_val(pos:itemmaxn-1)=D%char_val(pos+1:itemmaxn)
	case ('logical')
		D%logi_val(pos:itemmaxn-1)=D%logi_val(pos+1:itemmaxn)
	case ('tensor')
		D%ten_val(pos:itemmaxn-1)=D%ten_val(pos+1:itemmaxn)
	case ('int_ary')
		D%int_ary_val(:,pos:itemmaxn-1)=D%int_ary_val(:,pos+1:itemmaxn)
		D%int_ary_len(pos:itemmaxn-1)=D%int_ary_len(pos+1:itemmaxn)
	case ('dbl_ary')
		D%dbl_ary_val(:,pos:itemmaxn-1)=D%dbl_ary_val(:,pos+1:itemmaxn)
		D%dbl_ary_len(pos:itemmaxn-1)=D%dbl_ary_len(pos+1:itemmaxn)
	case ('char_ary')
		D%char_ary_val(:,pos:itemmaxn-1)=D%char_ary_val(:,pos+1:itemmaxn)
		D%char_ary_len(pos:itemmaxn-1)=D%char_ary_len(pos+1:itemmaxn)
	case ('logi_ary')
		D%logi_ary_val(:,pos:itemmaxn-1)=D%logi_ary_val(:,pos+1:itemmaxn)
		D%logi_ary_len(pos:itemmaxn-1)=D%logi_ary_len(pos+1:itemmaxn)
	end select

	D%names(pos:itemmaxn-1,type)=D%names(pos+1:itemmaxn,type)
	D%itemcurn(type)=D%itemcurn(type)-1

end subroutine

subroutine isetvalue(D,name,val,add_tag)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	integer,intent(in)::val
	logical,intent(in),optional::add_tag
	logical::add_tag_,existed
	integer::type,pos

	add_tag_=.false.
	if(present(add_tag))then
		add_tag_=add_tag
	end if

	call D%search(name,type,pos,existed)
	if( existed) then
		select case (type_name(type))
		case ('integer')
			D%int_val(pos)=val
		case ('double')
			D%double_val(pos)=val
		case ('complex')
			D%com_val(pos)=val
		case ('character')
			D%char_val(pos)=str(val)
		case default
			call wc_error_stop('para-setvalue','cannot change from int to '//trim(type_name(type)))
		end select
	else
		if(add_tag_)then
			call D%insert_int(name,val)
		else
			call wc_error_stop('para-setvalue','Var '//trim(name)//' not existes')
		end if
	end if

end subroutine

subroutine misetvalue(D,name,val,add_tag)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	integer,intent(in)::val(:)
	logical,intent(in),optional::add_tag
	logical::add_tag_,existed
	integer::type,pos

	if (size(val)>lenmax) then
		call wc_error_stop('para-setvalue','length of int_ary parameter reaches limit')
	end if

	add_tag_=.false.
	if(present(add_tag))then
		add_tag_=add_tag
	end if

	call D%search(name,type,pos,existed)
	if( existed) then
		select case (type_name(type))
		case ('int_ary')
			D%int_ary_val(:size(val),pos)=val
			D%int_ary_len(pos)=size(val)
		case default
			call wc_error_stop('para-setvalue','cannot change from int_ary to '//trim(type_name(type)))
		end select
	else
		if(add_tag_)then
			call D%insert_int_ary(name,val)
		else
			call wc_error_stop('para-setvalue','Var '//trim(name)//' not existes')
		end if
	end if

end subroutine

subroutine dsetvalue(D,name,val,add_tag)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	real(8),intent(in)::val
	integer::type,pos
	logical,intent(in),optional::add_tag
	logical::add_tag_,existed

	add_tag_=.false.
	if(present(add_tag))then
		add_tag_=add_tag
	end if

	call D%search(name,type,pos,existed)
	if( existed) then
		select case (type_name(type))
		case ('integer')
			D%int_val(pos)=val
		case ('double')
			D%double_val(pos)=val
		case ('complex')
			D%com_val(pos)=val
		case ('character')
			D%char_val(pos)=str(val)
		case default
			call wc_error_stop('para-setvalue','cannot change from real to '//trim(type_name(type)))
		end select
	else
		if(add_tag_)then
			call D%insert_dbl(name,val)
		else
			call wc_error_stop('para-setvalue','Var '//trim(name)//' not existes')
		end if
	end if

end subroutine

subroutine mdsetvalue(D,name,val,add_tag)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	real(8),intent(in)::val(:)
	integer::type,pos
	logical,intent(in),optional::add_tag
	logical::add_tag_,existed

	if (size(val)>lenmax) then
		call wc_error_stop('para-setvalue','length of dbl_ary parameter reaches limit')
	end if

	add_tag_=.false.
	if(present(add_tag))then
		add_tag_=add_tag
	end if

	call D%search(name,type,pos,existed)
	if( existed) then
		select case (type_name(type))
		case ('dbl_ary')
			D%dbl_ary_val(:size(val),pos)=val
			D%dbl_ary_len(pos)=size(val)
		case default
			call wc_error_stop('para-setvalue','cannot change from dbl_ary to '//trim(type_name(type)))
		end select
	else
		if(add_tag_)then
			call D%insert_dbl_ary(name,val)
		else
			call wc_error_stop('para-setvalue','Var '//trim(name)//' not existes')
		end if
	end if

end subroutine

subroutine zsetvalue(D,name,val,add_tag)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	complex(8),intent(in)::val
	integer::type,pos
	logical,intent(in),optional::add_tag
	logical::add_tag_,existed

	add_tag_=.false.
	if(present(add_tag))then
		add_tag_=add_tag
	end if

	call D%search(name,type,pos,existed)
	if( existed) then
		select case (type_name(type))
		case ('integer')
			D%int_val(pos)=val
		case ('double')
			D%double_val(pos)=val
		case ('complex')
			D%com_val(pos)=val
		case ('character')
			D%char_val(pos)=str(val)
		case default
			call wc_error_stop('para-setvalue','cannot change from complex to '//trim(type_name(type)))
		end select
	else
		if(add_tag_)then
			call D%insert_com(name,val)
		else
			call wc_error_stop('para-setvalue','Var '//trim(name)//' not existes')
		end if
	end if

end subroutine

subroutine asetvalue(D,name,val,add_tag)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val
	integer::type,pos
	logical,intent(in),optional::add_tag
	logical::add_tag_,existed

	add_tag_=.false.
	if(present(add_tag))then
		add_tag_=add_tag
	end if

	call D%search(name,type,pos,existed)
	if( existed) then
		select case (type_name(type))
		case ('character')
			D%char_val(pos)=val
		case default
			call wc_error_stop('para-setvalue','cannot change from char to '//trim(type_name(type)))
		end select
	else
		if(add_tag_)then
			call D%insert_char(name,val)
		else
			call wc_error_stop('para-setvalue','Var '//trim(name)//' not existes')
		end if
	end if
	
end subroutine

subroutine masetvalue(D,name,val,add_tag)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val(:)
	integer::type,pos
	logical,intent(in),optional::add_tag
	logical::add_tag_,existed

	if (size(val)>lenmax) then
		call wc_error_stop('para-setvalue','length of char_ary parameter reaches limit')
	end if

	add_tag_=.false.
	if(present(add_tag))then
		add_tag_=add_tag
	end if

	call D%search(name,type,pos,existed)
	if( existed) then
		select case (type_name(type))
		case ('char_ary')
			D%char_ary_val(:size(val),pos)=val
			D%char_ary_len(pos)=size(val)
		case default
			call wc_error_stop('para-setvalue','cannot change from char_ary to '//trim(type_name(type)))
		end select
	else
		if(add_tag_)then
			call D%insert_char_ary(name,val)
		else
			call wc_error_stop('para-setvalue','Var '//trim(name)//' not existes')
		end if
	end if
	
end subroutine

subroutine lsetvalue(D,name,val,add_tag)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	logical,intent(in)::val
	integer::type,pos
	logical,intent(in),optional::add_tag
	logical::add_tag_,existed

	add_tag_=.false.
	if(present(add_tag))then
		add_tag_=add_tag
	end if

	call D%search(name,type,pos,existed)
	if( existed) then
		call D%find(name,type,pos)
		select case (type_name(type))
		case ('character')
			D%char_val(pos)=val
		case ('logical')
			D%logi_val(pos)=val
		case default
			call wc_error_stop('para-setvalue','cannot change from logical to '//trim(type_name(type)))
		end select
	else
		if(add_tag_)then
			call D%insert_logi(name,val)
		else
			call wc_error_stop('para-setvalue','Var '//trim(name)//' not existes')
		end if
	end if

end subroutine

subroutine mlsetvalue(D,name,val,add_tag)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	logical,intent(in)::val(:)
	integer::type,pos
	logical,intent(in),optional::add_tag
	logical::add_tag_,existed

	if (size(val)>lenmax) then
		call wc_error_stop('para-setvalue','length of logi_ary parameter reaches limit')
	end if

	add_tag_=.false.
	if(present(add_tag))then
		add_tag_=add_tag
	end if

	call D%search(name,type,pos,existed)
	if( existed) then
		select case (type_name(type))
		case ('logi_ary')
			D%logi_ary_val(:size(val),pos)=val
			D%logi_ary_len(pos)=size(val)
		case default
			call wc_error_stop('para-setvalue','cannot change from logi_ary to '//trim(type_name(type)))
		end select
	else
		if(add_tag_)then
			call D%insert_logi_ary(name,val)
		else
			call wc_error_stop('para-setvalue','Var '//trim(name)//' not existes')
		end if
	end if

end subroutine

subroutine tsetvalue(D,name,val,add_tag)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	type(tensor),intent(in)::val
	integer::type,pos
	logical,intent(in),optional::add_tag
	logical::add_tag_,existed

	add_tag_=.false.
	if(present(add_tag))then
		add_tag_=add_tag
	end if

	call D%search(name,type,pos,existed)
	if( existed) then
		call D%find(name,type,pos)
		select case (type_name(type))
		case ('tensor')
			D%ten_val(pos)=val
		case default
			call wc_error_stop('para-setvalue','cannot change from tensor to '//trim(type_name(type)))
		end select
	else
		if(add_tag_)then
			call D%insert_ten(name,val)
		else
			call wc_error_stop('para-setvalue','Var '//trim(name)//' not existes')
		end if
	end if

end subroutine

subroutine igetvalue(D,name,val)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer,intent(inout)::val
	integer::type,pos

	call D%find(name,type,pos)

	select case (type_name(type))
	case ('integer')
		val=D%int_val(pos)
	case ('double')
		val=D%double_val(pos)
	case ('complex')
		val=D%com_val(pos)
	case default
		call wc_error_stop('para-getvalue','cannot change from '//trim(type_name(type))//'  to int')
	end select

end subroutine

subroutine migetvalue(D,name,val)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	integer,intent(inout),allocatable::val(:)
	integer::type,pos

	call D%find(name,type,pos)

	select case (type_name(type))
	case ('int_ary')
		allocate(val(D%int_ary_len(pos)))
		val=D%int_ary_val(:D%int_ary_len(pos),pos)
	case default
		call wc_error_stop('para-getvalue','cannot change from '//trim(type_name(type))//'  to int_ary')
	end select

end subroutine

subroutine dgetvalue(D,name,val)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	real(8),intent(inout)::val
	integer::type,pos

	call D%find(name,type,pos)
	select case (type_name(type))
	case ('integer')
		val=D%int_val(pos)
	case ('double')
		val=D%double_val(pos)
	case ('complex')
		val=D%com_val(pos)
	case default
		call wc_error_stop('para-getvalue','cannot change from '//trim(type_name(type))//'  to real')
	end select

end subroutine

subroutine mdgetvalue(D,name,val)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	real(8),intent(inout),allocatable::val(:)
	integer::type,pos

	call D%find(name,type,pos)
	select case (type_name(type))
	case ('dbl_ary')
		allocate(val(D%dbl_ary_len(pos)))
		val=D%dbl_ary_val(:D%dbl_ary_len(pos),pos)
	case default
		call wc_error_stop('para-getvalue','cannot change from '//trim(type_name(type))//'  to dbl_ary')
	end select

end subroutine

subroutine zgetvalue(D,name,val)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	complex(8),intent(inout)::val
	integer::type,pos

	call D%find(name,type,pos)
	select case (type_name(type))
	case ('integer')
		val=D%int_val(pos)
	case ('double')
		val=D%double_val(pos)
	case ('complex')
		val=D%com_val(pos)
	case default
		call wc_error_stop('para-getvalue','cannot change from '//trim(type_name(type))//'  to complex')
	end select

end subroutine

subroutine agetvalue(D,name,val)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	character(len=*),intent(inout)::val
	integer::type,pos

	call D%find(name,type,pos)

	select case (type_name(type))
	case ('integer')
		val=str(D%int_val(pos))
	case ('double')
		val=str(D%double_val(pos))
	case ('complex')
		val=str(D%com_val(pos))
	case ('character')
		val=D%char_val(pos)
	case ('logical')
		val=str(D%logi_val(pos))
	case ('char_ary')
		val=str(D%char_ary_val(:,pos))
	case default
		write(*,*)type
		call wc_error_stop('para-getvalue','cannot change from '//trim(type_name(type))//'  to char')
	end select
	
end subroutine

subroutine magetvalue(D,name,val)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	character(len=max_char_length),intent(inout),allocatable::val(:)
	integer::type,pos

	call D%find(name,type,pos)

	select case (type_name(type))
	case ('char_ary')
		allocate(val(D%char_ary_len(pos)))
		val=D%char_ary_val(:D%char_ary_len(pos),pos)
	case default
		call wc_error_stop('para-getvalue','cannot change from '//trim(type_name(type))//'  to char_ary')
	end select
	
end subroutine

subroutine lgetvalue(D,name,val)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	logical,intent(inout)::val
	integer::type,pos

	call D%find(name,type,pos)
	select case (type_name(type))
	case ('logical')
		val=D%logi_val(pos)
	case default
		call wc_error_stop('para-getvalue','cannot change from '//trim(type_name(type))//'  to logical')
	end select

end subroutine

subroutine mlgetvalue(D,name,val)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	logical,intent(inout),allocatable::val(:)
	integer::type,pos

	call D%find(name,type,pos)
	select case (type_name(type))
	case ('logi_ary')
		allocate(val(D%logi_ary_len(pos)))
		val=D%logi_ary_val(:D%logi_ary_len(pos),pos)
	case default
		call wc_error_stop('para-getvalue','cannot change from '//trim(type_name(type))//'  to logi_ary')
	end select

end subroutine

subroutine tgetvalue(D,name,val)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::name
	type(tensor),intent(inout)::val
	integer::type,pos

	call D%find(name,type,pos)
	select case (type_name(type))
	case ('tensor')
		val=D%ten_val(pos)
	case default
		call wc_error_stop('para-getvalue','cannot change from '//trim(type_name(type))//'  to tensor')
	end select

end subroutine

subroutine rename(D,name,new_name)

	class(dictionary),intent(inout)::D
	character(len=*),intent(in)::name
	character(len=*),intent(inout)::new_name
	integer::type,pos

	call D%find(name,type,pos)
	D%names(pos,type)=new_name

end subroutine

subroutine append_by_name(D,D2,names)

	class(dictionary),intent(inout)::D
	class(dictionary),intent(in)::D2
	character(len=*),intent(in)::names(:)
	type(dictionary)::out_list
	integer::i,type,pos

	do i=1,size(names)
		call D2%find(names(i),type,pos)
		select case (type_name(type))
		case ('integer')
			call D%insert(names(i),D2%int_val(pos))
		case ('int_ary')
			call D%insert(names(i),D2%int_ary_val(:D2%int_ary_len(pos),pos))
		case ('double')
			call D%insert(names(i),D2%double_val(pos))
		case ('dbl_ary')
			call D%insert(names(i),D2%dbl_ary_val(:D2%dbl_ary_len(pos),pos))
		case ('complex')
			call D%insert(names(i),D2%com_val(pos))
		case ('character')
			call D%insert(names(i),D2%char_val(pos))
		case ('char_ary')
			call D%insert(names(i),D2%char_ary_val(:D2%char_ary_len(pos),pos))
		case ('logical')
			call D%insert(names(i),D2%logi_val(pos))
		case ('logi_ary')
			call D%insert(names(i),D2%logi_ary_val(:D2%logi_ary_len(pos),pos))
		case ('tensor')
			call D%insert(names(i),D2%ten_val(pos))
		end select
	end do

end subroutine

function sub_name(D,names) result(out_list)

	class(dictionary),intent(in)::D
	character(len=*),intent(in)::names(:)
	type(dictionary)::out_list

	if(sum(D%itemcurn)>0) then
		call out_list%append_by_name(D,names)
	end if

end function

subroutine print(D,unit,end_tag_)

	class(dictionary),intent(in)::D
	integer,intent(in),optional::unit
	logical,intent(in),optional::end_tag_
	logical::end_tag
	integer::k,i,type
	character(len=max_char_length+20):: formatl

	end_tag=.true.
	if(present(end_tag_)) end_tag=end_tag_

	if (.not. D%inited) then
		if(present(unit) .and. end_tag) write(unit,*) '/'
		return 
	end if

	do type=1,type_num
		do k=1,D%itemcurn(type)
			formatl=type_abbr(type)
			formatl(4:)=D%names(k,type)
			select case(type_name(type))
			case('integer')
				formatl(20:)=str(D%int_val(k))
			case('double')
				formatl(20:)=str(D%double_val(k))
			case('complex')
				formatl(20:)=str(D%com_val(k))
			case('character')
				formatl(20:)=D%char_val(k)
			case('logical')
				formatl(20:)=str(D%logi_val(k))
			case('tensor')
				formatl(20:)='printed below'
			case('int_ary')
				formatl(20:)=str(D%int_ary_len(k))//' items:'
			case('dbl_ary')
				formatl(20:)=str(D%dbl_ary_len(k))//' items:'
			case('char_ary')
				formatl(20:)=str(D%char_ary_len(k))//' items:'
			case('logi_ary')
				formatl(20:)=str(D%logi_ary_len(k))//' items:'
			end select
			if(present(unit))then
				write(unit,*) trim(formatl)
			else
				call writemess(formatl)
			end if

			select case(type_name(type))
			case('tensor')
				if(present(unit))then
					call D%ten_val(k)%write(unit)
				else
					call D%ten_val(k)%print()
				end if
			case('int_ary')
				do i=1,D%int_ary_len(k)
					formatl=''
					formatl(20:)=str(D%int_ary_val(i,k))
					if(present(unit))then
						write(unit,*) trim(formatl)
					else
						call writemess(formatl)
					end if
				end do
			case('dbl_ary')
				do i=1,D%dbl_ary_len(k)
					formatl=''
					formatl(20:)=str(D%dbl_ary_val(i,k))
					if(present(unit))then
						write(unit,*) trim(formatl)
					else
						call writemess(formatl)
					end if
				end do
			case('char_ary')
				do i=1,D%char_ary_len(k)
					formatl=''
					formatl(20:)=D%char_ary_val(i,k)
					if(present(unit))then
						write(unit,*) trim(formatl)
					else
						call writemess(formatl)
					end if
				end do
			case('logi_ary')
				do i=1,D%logi_ary_len(k)
					formatl=''
					formatl(20:)=str(D%logi_ary_val(i,k))
					if(present(unit))then
						write(unit,*) trim(formatl)
					else
						call writemess(formatl)
					end if
				end do
			end select
		end do
	end do

	if(present(unit) .and. end_tag) write(unit,*) '/'

end subroutine

subroutine read(G,unit)

	class(dictionary),intent(inout)::G
	integer,intent(in)::unit
	character(len=max_char_length)::cur_class
	character(len=10*max_char_length)::line
	character(len=4)::type
	integer::io_stat,len,i,pos

	character(len=max_char_length)::no_use,cur_name
	integer::ival 
	real(8)::dval 
	complex(8)::zval 
	character(len=max_char_length)::aval 
	logical::lval 
	type(tensor)::tval
	integer,allocatable::mival(:)
	real(8),allocatable::mdval(:)
	character(len=max_char_length),allocatable::maval(:)
	logical,allocatable::mlval(:)

	if (.not. G%inited) call G%init()
	call G%clean()

	read(unit,'(A)',IOSTAT=io_stat) line
	do while(.true.)
		if(io_stat/=0) call wc_error_stop('dictionary.read','EOF before identifier "/"')
		line=adjustl(line)
		type=''
		if(len_trim(line)>0) then
			pos=scan(line,' '//achar(9)) ! achar(9) for tab
			type=line(1:pos-1)
		end if
		select case(type)
		case('i')
			read(line(pos:),*) cur_name,ival
			call G%insert(cur_name,ival)
		case('d')
			read(line(pos:),*) cur_name,dval
			call G%insert(cur_name,dval)
		case('z')
			read(line(pos:),*) cur_name,zval
			call G%insert(cur_name,zval)
		case('a')
			read(line(pos:),*) cur_name,aval
			call G%insert(cur_name,aval)
		case('l')
			read(line(pos:),*) cur_name,lval
			call G%insert(cur_name,lval)
		case('t')
			read(line(pos:),*) cur_name
			call tval%read(unit)
			call G%insert(cur_name,tval)
		case('mi')
			read(line(pos:),*)cur_name,len
			if(allocated(mival)) deallocate(mival)
			allocate(mival(len))
			do i=1,len
				read(unit,*,IOSTAT=io_stat) mival(i)
				if(io_stat/=0) call wc_error_stop('dictionary.read','array reading stopped')
			end do
			call G%insert(cur_name,mival)
		case('md')
			read(line(pos:),*)cur_name,len
			if(allocated(mdval)) deallocate(mdval)
			allocate(mdval(len))
			do i=1,len
				read(unit,*,IOSTAT=io_stat) mdval(i)
				if(io_stat/=0) call wc_error_stop('dictionary.read','array reading stopped')
			end do
			call G%insert(cur_name,mdval)
		case('ma')
			read(line(pos:),*)cur_name,len
			if(allocated(maval)) deallocate(maval)
			allocate(maval(len))
			do i=1,len
				read(unit,*,IOSTAT=io_stat) maval(i)
				if(io_stat/=0) call wc_error_stop('dictionary.read','array reading stopped')
			end do
			call G%insert(cur_name,maval)
		case('ml')
			read(line(pos:),*)cur_name,len
			if(allocated(mlval)) deallocate(mlval)
			allocate(mlval(len))
			do i=1,len
				read(unit,*,IOSTAT=io_stat) mlval(i)
				if(io_stat/=0) call wc_error_stop('dictionary.read','array reading stopped')
			end do
			call G%insert(cur_name,mlval)
		case('/')
			exit
		end select

		read(unit,'(A)',IOSTAT=io_stat) line
	end do

end subroutine

subroutine append(P1,P2)

	class(dictionary),intent(inout)::P1
	type(dictionary),intent(in)::P2

	if(sum(P2%itemcurn)>0) then
		call P1%append_by_name(P2,P2%get_names())
	end if

end subroutine

end module 