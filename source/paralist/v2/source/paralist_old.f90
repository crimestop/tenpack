module parameter_list
use error
use string
implicit none
private

	integer,parameter::itemmaxn=20
	type para_list

		private
		integer::int_val(itemmaxn)=0
		character(len=max_char_length) :: int_name(itemmaxn)=''
		character(len=max_char_length) :: int_class(itemmaxn)=''
		real(8)::real_val(itemmaxn)=0
		character(len=max_char_length) :: real_name(itemmaxn)=''
		character(len=max_char_length) :: real_class(itemmaxn)=''
		complex(8)::com_val(itemmaxn)=0
		character(len=max_char_length) :: com_name(itemmaxn)=''
		character(len=max_char_length) :: com_class(itemmaxn)=''
		character(len=max_char_length) :: char_val(itemmaxn)=''
		character(len=max_char_length) :: char_name(itemmaxn)=''
		character(len=max_char_length) :: char_class(itemmaxn)=''
		logical :: logi_val(itemmaxn)=.false.
		character(len=max_char_length) :: logi_name(itemmaxn)=''
		character(len=max_char_length) :: logi_class(itemmaxn)=''
		integer::itemcurn(5)=0

	contains

		private
		procedure,public::read
		procedure,public::print
		procedure::print_class
		procedure,public::sub_name
		procedure,public::sub_class
		procedure,public::clean
		procedure,public::rename
		procedure::isetvalue,dsetvalue,zsetvalue,asetvalue,lsetvalue
		generic,public::setvalue=>isetvalue,dsetvalue,zsetvalue,asetvalue,lsetvalue
		procedure,public::setclass
		procedure::insert_int,insert_real,insert_com,insert_char,insert_logi
		generic,public::insert=>insert_int,insert_real,insert_com,insert_char,insert_logi
		procedure,public::delete
		procedure,public::ii
		procedure,public::di
		procedure,public::zi
		procedure,public::ai
		procedure,public::li
		procedure,public::append
		procedure::find

	end type

	public para_list

contains

subroutine clean(D)
implicit none

	class(para_list), intent(inout)::D

	D%int_val=0
	D%int_name=''
	D%int_class=''
	D%real_val=0
	D%real_name=''
	D%real_class=''
	D%com_val=0
	D%com_name=''
	D%com_class=''
	D%char_val=''
	D%char_name=''
	D%char_class=''
	D%logi_val=.false.
	D%logi_name=''
	D%logi_class=''
	D%itemcurn=0

end subroutine

subroutine insert_int(D,name,value,class_)
implicit none

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class_
	integer, intent(in) :: value
	character(len=100)::class
	integer::type

	if(present(class_))then
		class=class_
	else
		class='none'
	end if

		type=1
		if(D%itemcurn(type)==itemmaxn)then
			call wc_error_stop('para-insert','num of integer parameter reaches limit')
		else
			D%itemcurn(type)=D%itemcurn(type)+1
			D%int_val(D%itemcurn(type))=value
			D%int_name(D%itemcurn(type))=name
			D%int_class(D%itemcurn(type))=class
		end if
end subroutine


subroutine insert_real(D,name,value,class_)
implicit none

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class_
	real(8), intent(in) :: value
	character(len=100)::class
	integer::type

	if(present(class_))then
		class=class_
	else
		class='none'
	end if
		type=2
		if(D%itemcurn(type)==itemmaxn)then
			call wc_error_stop('para-insert','num of real parameter reaches limit')
		else
			D%itemcurn(type)=D%itemcurn(type)+1
			D%real_val(D%itemcurn(type))=value
			D%real_name(D%itemcurn(type))=name
			D%real_class(D%itemcurn(type))=class
		end if
end subroutine

subroutine insert_com(D,name,value,class_)
implicit none

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class_
	complex(8), intent(in) :: value
	character(len=100)::class
	integer::type

	if(present(class_))then
		class=class_
	else
		class='none'
	end if

		type=3
		if(D%itemcurn(type)==itemmaxn)then
			call wc_error_stop('para-insert','num of complex parameter reaches limit')
		else
			D%itemcurn(type)=D%itemcurn(type)+1
			D%com_val(D%itemcurn(type))=value
			D%com_name(D%itemcurn(type))=name
			D%com_class(D%itemcurn(type))=class
		end if


end subroutine

subroutine insert_char(D,name,value,class_)
implicit none

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class_
	character(len=*), intent(in) :: value
	character(len=100)::class
	integer::type

	if(present(class_))then
		class=class_
	else
		class='none'
	end if

		type=4
		if(D%itemcurn(type)==itemmaxn)then
			call wc_error_stop('para-insert','num of character parameter reaches limit')
		else
			D%itemcurn(type)=D%itemcurn(type)+1
			D%char_val(D%itemcurn(type))=value
			D%char_name(D%itemcurn(type))=name
			D%char_class(D%itemcurn(type))=class
		end if

end subroutine

subroutine insert_logi(D,name,value,class_)
implicit none

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class_
	logical, intent(in) :: value
	character(len=100)::class
	integer::type

	if(present(class_))then
		class=class_
	else
		class='none'
	end if
	
		type=5
		if(D%itemcurn(type)==itemmaxn)then
			call wc_error_stop('para-insert','num of logical parameter reaches limit')
		else
			D%itemcurn(type)=D%itemcurn(type)+1
			D%logi_val(D%itemcurn(type))=value
			D%logi_name(D%itemcurn(type))=name
			D%logi_class(D%itemcurn(type))=class
		end if

end subroutine

integer function ii(D,name) result(res)
implicit none

	class(para_list),intent(in)::D
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
	end select

end function

real function di(D,name) result(res)
implicit none

	class(para_list),intent(in)::D
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
	end select

end function

complex function zi(D,name) result(res)
implicit none

	class(para_list),intent(in)::D
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
	end select

end function

character(len=max_char_length) function ai(D,name) result(res)
implicit none

	class(para_list),intent(in)::D
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
	end select

end function

logical function li(D,name) result(res)
implicit none

	class(para_list),intent(in)::D
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
	end select

end function

subroutine find(D,name,type,pos)
implicit none

	class(para_list),intent(in)::D
	character(len=*),intent(in)::name
	integer,intent(out)::type,pos
	integer::i

	type=0

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

	if(type==0)then
		call wc_error_stop('paralist.find',trim(name)//' not found in the paralist.')
	end if 

end subroutine

subroutine delete(D,name)
implicit none

	class(para_list),intent(inout)::D
	character(len=*),intent(in)::name
	integer::type,pos

	call D%find(name,type,pos)

	select case (type)
	case (1)
		D%int_val(pos:itemmaxn-1)=D%int_val(pos+1:itemmaxn)
		D%int_name(pos:itemmaxn-1)=D%int_name(pos+1:itemmaxn)
		D%int_class(pos:itemmaxn-1)=D%int_class(pos+1:itemmaxn)
	case (2)
		D%real_val(pos:itemmaxn-1)=D%real_val(pos+1:itemmaxn)
		D%real_name(pos:itemmaxn-1)=D%real_name(pos+1:itemmaxn)
		D%real_class(pos:itemmaxn-1)=D%real_class(pos+1:itemmaxn)
	case (3)
		D%com_val(pos:itemmaxn-1)=D%com_val(pos+1:itemmaxn)
		D%com_name(pos:itemmaxn-1)=D%com_name(pos+1:itemmaxn)
		D%com_class(pos:itemmaxn-1)=D%com_class(pos+1:itemmaxn)
	case (4)
		D%char_val(pos:itemmaxn-1)=D%char_val(pos+1:itemmaxn)
		D%char_name(pos:itemmaxn-1)=D%char_name(pos+1:itemmaxn)
		D%char_class(pos:itemmaxn-1)=D%char_class(pos+1:itemmaxn)
	case (5)
		D%logi_val(pos:itemmaxn-1)=D%logi_val(pos+1:itemmaxn)
		D%logi_name(pos:itemmaxn-1)=D%logi_name(pos+1:itemmaxn)
		D%logi_class(pos:itemmaxn-1)=D%logi_class(pos+1:itemmaxn)
	end select

	D%itemcurn(type)=D%itemcurn(type)-1

end subroutine

subroutine isetvalue(D,name,val)
implicit none

	class(para_list),intent(inout)::D
	character(len=*),intent(in)::name
	integer,intent(in)::val
	integer::type,pos

	call D%find(name,type,pos)

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
	end select

end subroutine

subroutine dsetvalue(D,name,val)
implicit none

	class(para_list),intent(inout)::D
	character(len=*),intent(in)::name
	real(8),intent(in)::val
	integer::type,pos

	call D%find(name,type,pos)
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
	end select

end subroutine

subroutine zsetvalue(D,name,val)
implicit none

	class(para_list),intent(inout)::D
	character(len=*),intent(in)::name
	complex(8),intent(in)::val
	integer::type,pos

	call D%find(name,type,pos)
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
	end select

end subroutine

subroutine asetvalue(D,name,val)
implicit none

	class(para_list),intent(inout)::D
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val
	integer::type,pos

	call D%find(name,type,pos)

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
	end select
	
end subroutine

subroutine lsetvalue(D,name,val)
implicit none

	class(para_list),intent(inout)::D
	character(len=*),intent(in)::name
	logical,intent(in)::val
	integer::type,pos

	call D%find(name,type,pos)
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
	end select

end subroutine

subroutine rename(D,name,new_name)
implicit none

	class(para_list),intent(inout)::D
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
	end select

end subroutine

subroutine setclass(D,name,new_class)
implicit none

	class(para_list),intent(inout)::D
	character(len=*),intent(in)::name
	character(len=*),intent(inout)::new_class
	integer::type,pos

	call D%find(name,type,pos)

	select case (type)
	case (1)
		D%int_class(pos)=new_class
	case (2)
		D%real_class(pos)=new_class
	case (3)
		D%com_class(pos)=new_class
	case (4)
		D%char_class(pos)=new_class
	case (5)
		D%logi_class(pos)=new_class
	end select

end subroutine

type(para_list) function sub_name(D,names) result(out_list)
implicit none

	class(para_list),intent(in)::D
	character(len=*),intent(in)::names(:)
	integer::i
	integer::type,pos

	do i=1,size(names)
		call D%find(names(i),type,pos)
		select case (type)
		case (1)
			call out_list%insert(names(i),D%int_val(pos),D%int_class(pos))
		case (2)
			call out_list%insert(names(i),D%real_val(pos),D%real_class(pos))
		case (3)
			call out_list%insert(names(i),D%com_val(pos),D%com_class(pos))
		case (4)
			call out_list%insert(names(i),D%char_val(pos),D%char_class(pos))
		case (5)
			call out_list%insert(names(i),D%logi_val(pos),D%logi_class(pos))
		end select
	end do

end function

type(para_list) function sub_class(D,class) result(out_list)
implicit none

	class(para_list),intent(in)::D
	character(len=*),intent(in)::class
	integer::i

	do i=1,D%itemcurn(1)
		if(D%int_class(i) == class) then
			call out_list%insert(D%int_name(i),D%int_val(i),class)
		end if
	end do

	do i=1,D%itemcurn(2)
		if(D%real_class(i) == class) then
			call out_list%insert(D%real_name(i),D%real_val(i),class)
		end if
	end do

	do i=1,D%itemcurn(3)
		if(D%com_class(i) == class) then
			call out_list%insert(D%com_name(i),D%com_val(i),class)
		end if
	end do

	do i=1,D%itemcurn(4)
		if(D%char_class(i) == class) then
			call out_list%insert(D%char_name(i),D%char_val(i),class)
		end if
	end do

	do i=1,D%itemcurn(5)
		if(D%logi_class(i) == class) then
			call out_list%insert(D%logi_name(i),D%logi_val(i),class)
		end if
	end do

end function

subroutine print(D,unit)
implicit none

	class(para_list),intent(in)::D
	integer,intent(in),optional::unit
	character(len=max_char_length)::finish_class(itemmaxn*5)
	character(len=max_char_length)::class_name
	integer::i,j,finish_class_num
	logical::exist,none_tag=.false.

	finish_class=''
	finish_class_num=0
	do i=1,D%itemcurn(1)
		class_name=D%int_class(i)
		if(class_name .nequ. 'none')then
			exist=.false.
			do j=1,finish_class_num
				if (finish_class(j) == class_name)then
					exist=.true.
					exit 
				end if
			end do
			if(.not. exist)then
				finish_class_num=finish_class_num+1
				finish_class(finish_class_num)=class_name
				call D%print_class(class_name,unit)
			end if
		else
			none_tag=.true.
		end if
	end do

	do i=1,D%itemcurn(2)
		class_name=D%real_class(i)
		if(class_name .nequ. 'none')then
			exist=.false.
			do j=1,finish_class_num
				if (finish_class(j) == class_name)then
					exist=.true.
					exit 
				end if
			end do
			if(.not. exist)then
				finish_class_num=finish_class_num+1
				finish_class(finish_class_num)=class_name
				call D%print_class(class_name,unit)
			end if
		else
			none_tag=.true.
		end if
	end do

	do i=1,D%itemcurn(3)
		class_name=D%com_class(i)
		if(class_name .nequ. 'none')then
			exist=.false.
			do j=1,finish_class_num
				if (finish_class(j) == class_name)then
					exist=.true.
					exit 
				end if
			end do
			if(.not. exist)then
				finish_class_num=finish_class_num+1
				finish_class(finish_class_num)=class_name
				call D%print_class(class_name,unit)
			end if
		else
			none_tag=.true.
		end if
	end do

	do i=1,D%itemcurn(4)
		class_name=D%char_class(i)
		if(class_name .nequ. 'none')then
			exist=.false.
			do j=1,finish_class_num
				if (finish_class(j) == class_name)then
					exist=.true.
					exit 
				end if
			end do
			if(.not. exist)then
				finish_class_num=finish_class_num+1
				finish_class(finish_class_num)=class_name
				call D%print_class(class_name,unit)
			end if
		else
			none_tag=.true.
		end if
	end do

	do i=1,D%itemcurn(5)
		class_name=D%logi_class(i)
		if(class_name .nequ. 'none')then
			exist=.false.
			do j=1,finish_class_num
				if (finish_class(j) == class_name)then
					exist=.true.
					exit 
				end if
			end do
			if(.not. exist)then
				finish_class_num=finish_class_num+1
				finish_class(finish_class_num)=class_name
				call D%print_class(class_name,unit)
			end if
		else
			none_tag=.true.
		end if
	end do

	if(none_tag)then
		class_name='none'
		exist=.false.
		do j=1,finish_class_num
			if (finish_class(j) == class_name)then
				exist=.true.
				exit 
			end if
		end do
		if(.not. exist)then
			finish_class_num=finish_class_num+1
			finish_class(finish_class_num)=class_name
			call D%print_class(class_name,unit)
		end if
	end if

end subroutine

subroutine print_class(D,class_name,unit)
implicit none

	class(para_list),intent(in)::D
	character(len=*)::class_name
	integer,intent(in),optional::unit
	integer::k
	character(len=max_char_length+20):: formatl

	if(present(unit))then
		write(unit,*) '# Class '// trim(class_name)
	else
		call writemess('# Class '// class_name)
	end if

	do k=1,D%itemcurn(1)
		if(D%int_class(k)==class_name)then
			formatl='i'
			formatl(3:)=D%int_name(k)
			formatl(20:)=str(D%int_val(k))
			if(present(unit))then
				write(unit,*) trim(formatl)
			else
				call writemess(formatl)
			end if
		end if
	end do

	do k=1,D%itemcurn(2)
		if(D%real_class(k)==class_name)then
			formatl='d'
			formatl(3:)=D%real_name(k)
			formatl(20:)=str(D%real_val(k))
			if(present(unit))then
				write(unit,*) trim(formatl)
			else
				call writemess(formatl)
			end if
		end if
	end do

	do k=1,D%itemcurn(3)
		if(D%com_class(k)==class_name)then
			formatl='z'
			formatl(3:)=D%com_name(k)
			formatl(20:)=str(D%com_val(k))
			if(present(unit))then
				write(unit,*) trim(formatl)
			else
				call writemess(formatl)
			end if
		end if
	end do

	do k=1,D%itemcurn(4)
		if(D%char_class(k)==class_name)then
			formatl='a'
			formatl(3:)=D%char_name(k)
			formatl(20:)=D%char_val(k)
			if(present(unit))then
				write(unit,*) trim(formatl)
			else
				call writemess(formatl)
			end if
		end if
	end do

	do k=1,D%itemcurn(5)
		if(D%logi_class(k)==class_name)then
			formatl='l'
			formatl(3:)=D%logi_name(k)
			formatl(20:)=str(D%logi_val(k))
			if(present(unit))then
				write(unit,*) trim(formatl)
			else
				call writemess(formatl)
			end if
		end if
	end do	

	if(present(unit))then
		write(unit,*)'#'
		write(unit,*)
	else
		call writemess('#')
		call writemess('')
	end if

end subroutine

subroutine read(G,unit)
implicit none

	class(para_list),intent(inout)::G
	integer,intent(in)::unit
	character(len=max_char_length)::cur_class
	character(len=10*max_char_length)::line
	integer::io_stat
	logical :: class_on

	character(len=max_char_length)::no_use,cur_name
	integer::ival 
	real(8)::dval 
	complex(8)::zval 
	character(len=max_char_length)::aval 
	logical::lval 

	call G%clean()

	cur_class='none'
	class_on=.false.
	read(unit,'(A)',IOSTAT=io_stat) line
	do while(io_stat==0)
		line=adjustl(line)
		select case(line(1:1))
		case('#')
			if(.not. class_on) then
				read(line(2:),*) no_use,cur_class
				class_on=.true.
			else
				cur_class='none'
				class_on=.false.
			end if
		case('i')
			read(line(2:),*) cur_name,ival
			call G%insert(cur_name,ival,cur_class)
		case('d')
			read(line(2:),*) cur_name,dval
			call G%insert(cur_name,dval,cur_class)
		case('z')
			read(line(2:),*) cur_name,zval
			call G%insert(cur_name,zval,cur_class)
		case('a')
			read(line(2:),*) cur_name,aval
			call G%insert(cur_name,aval,cur_class)
		case('l')
			read(line(2:),*) cur_name,lval
			call G%insert(cur_name,lval,cur_class)
		end select

		read(unit,'(A)',IOSTAT=io_stat) line
	end do

end subroutine

subroutine append(P1,P2)

	class(para_list),intent(inout)::P1
	class(para_list),intent(in)::P2
	integer::k

	do k=1,P2%itemcurn(1)
		call P1%insert(P2%int_name(k),P2%int_val(k),P2%int_class(k))
	end do

	do k=1,P2%itemcurn(2)
		call P1%insert(P2%real_name(k),P2%real_val(k),P2%real_class(k))
	end do

	do k=1,P2%itemcurn(3)
		call P1%insert(P2%com_name(k),P2%com_val(k),P2%com_class(k))
	end do

	do k=1,P2%itemcurn(4)
		call P1%insert(P2%char_name(k),P2%char_val(k),P2%char_class(k))
	end do

	do k=1,P2%itemcurn(5)
		call P1%insert(P2%logi_name(k),P2%logi_val(k),P2%logi_class(k))
	end do	

end subroutine

end module 