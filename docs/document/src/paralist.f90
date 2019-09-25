module parameter_list
use string
use error
use mod_mpi_info
use mod_dictionary
implicit none
private

type, extends(dictionary):: para_list

	private
	type(dictionary)::dic_class

contains

	private
	procedure,public::read
	procedure,public::print
	procedure::print_class
	procedure,public::subpara_name
	procedure,public::subpara_class
	procedure,public::clean
	procedure,public::rename
	procedure,public::setclass
	procedure::pinsert_int,pinsert_real,pinsert_com,pinsert_char,pinsert_logi,&
		pinsert_int_ary,pinsert_real_ary,pinsert_char_ary,pinsert_logi_ary
	generic,public::pinsert=>pinsert_int,pinsert_real,pinsert_com,pinsert_char,pinsert_logi,&
		pinsert_int_ary,pinsert_real_ary,pinsert_char_ary,pinsert_logi_ary
	procedure,public::delete
	procedure,public::append_para

end type

public para_list

contains

subroutine clean(D)

	class(para_list), intent(inout)::D

	call D%dictionary%clean()
	call D%dic_class%clean()

end subroutine

subroutine pinsert_int(D,name,value,class)

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class
	integer, intent(in) :: value

	call D%dictionary%insert(name,value)
	if(present(class))then
		call D%dic_class%insert(name,class)
	else
		call D%dic_class%insert(name,'none')
	end if

end subroutine

subroutine pinsert_int_ary(D,name,value,class)

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class
	integer, intent(in) :: value(:)

	call D%dictionary%insert(name,value)
	if(present(class))then
		call D%dic_class%insert(name,class)
	else
		call D%dic_class%insert(name,'none')
	end if

end subroutine

subroutine pinsert_real(D,name,value,class)

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class
	real(8), intent(in) :: value

	call D%dictionary%insert(name,value)
	if(present(class))then
		call D%dic_class%insert(name,class)
	else
		call D%dic_class%insert(name,'none')
	end if

end subroutine

subroutine pinsert_real_ary(D,name,value,class)

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class
	real(8), intent(in) :: value(:)

	call D%dictionary%insert(name,value)
	if(present(class))then
		call D%dic_class%insert(name,class)
	else
		call D%dic_class%insert(name,'none')
	end if

end subroutine

subroutine pinsert_com(D,name,value,class)

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class
	complex(8), intent(in) :: value

	call D%dictionary%insert(name,value)
	if(present(class))then
		call D%dic_class%insert(name,class)
	else
		call D%dic_class%insert(name,'none')
	end if

end subroutine

subroutine pinsert_char(D,name,value,class)

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class
	character(len=*), intent(in) :: value

	call D%dictionary%insert(name,value)
	if(present(class))then
		call D%dic_class%insert(name,class)
	else
		call D%dic_class%insert(name,'none')
	end if

end subroutine

subroutine pinsert_char_ary(D,name,value,class)

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class
	character(len=*), intent(in) :: value(:)

	call D%dictionary%insert(name,value)
	if(present(class))then
		call D%dic_class%insert(name,class)
	else
		call D%dic_class%insert(name,'none')
	end if

end subroutine

subroutine pinsert_logi(D,name,value,class)

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class
	logical, intent(in) :: value

	call D%dictionary%insert(name,value)
	if(present(class))then
		call D%dic_class%insert(name,class)
	else
		call D%dic_class%insert(name,'none')
	end if

end subroutine

subroutine pinsert_logi_ary(D,name,value,class)

	class(para_list), intent(inout)::D
	character(len=*), intent(in) :: name
	character(len=*), intent(in),optional :: class
	logical, intent(in) :: value(:)

	call D%dictionary%insert(name,value)
	if(present(class))then
		call D%dic_class%insert(name,class)
	else
		call D%dic_class%insert(name,'none')
	end if

end subroutine

subroutine delete(D,name)

	class(para_list),intent(inout)::D
	character(len=*),intent(in)::name

	call D%dictionary%delete(name)
	call D%dic_class%delete(name)

end subroutine

subroutine rename(D,name,new_name)

	class(para_list),intent(inout)::D
	character(len=*),intent(in)::name
	character(len=*),intent(inout)::new_name

	call D%dictionary%rename(name,new_name)
	call D%dic_class%rename(name,new_name)

end subroutine

subroutine setclass(D,name,new_class)

	class(para_list),intent(inout)::D
	character(len=*),intent(in)::name
	character(len=*),intent(inout)::new_class

	call D%dic_class%setvalue(name,new_class)

end subroutine

function subpara_name(D,names) result(out_list)

	class(para_list),intent(in)::D
	character(len=*),intent(in)::names(:)
	type(para_list)::out_list

	out_list%dictionary=D%dictionary%sub_name(names)
	out_list%dic_class=D%dic_class%sub_name(names)

end function

function subpara_class(D,class) result(out_list)

	class(para_list),intent(in)::D
	character(len=*),intent(in)::class
	type(para_list)::out_list

	out_list%dic_class=D%dic_class%sub_val(class)
	out_list%dictionary=D%dictionary%sub_name(out_list%dic_class%get_names())

end function

subroutine print(D,unit,end_tag_)

	class(para_list),intent(in)::D
	integer,intent(in),optional::unit
	logical,intent(in),optional::end_tag_
	logical::end_tag
	character(len=max_char_length),allocatable::finish_class(:)
	character(len=max_char_length),allocatable::item_name(:)
	character(len=max_char_length)::class_name
	integer::i,j,finish_class_num
	logical::none_tag

	end_tag=.true.
	if(present(end_tag_)) end_tag=end_tag_

	none_tag=.false.
	item_name=D%dic_class%get_names()
	allocate(finish_class(size(item_name)))
	finish_class=''
	finish_class_num=0
	do i=1,size(item_name)
		class_name=D%dic_class%ai(item_name(i) )
		if(.not.(class_name == 'none'))then
			if (.not. any(finish_class(:finish_class_num)== class_name))then
				finish_class_num=finish_class_num+1
				finish_class(finish_class_num)=class_name
				call D%print_class(class_name,unit)
			end if
		else
			none_tag=.true.
		end if
	end do

	if(none_tag) call D%print_class('none',unit)
	if(present(unit) .and. end_tag) write(unit,*) '/'

end subroutine

subroutine print_class(D,class_name,unit)

	class(para_list),intent(in)::D
	type(para_list)::sub_para
	character(len=*)::class_name
	integer,intent(in),optional::unit

	if(present(unit))then
		write(unit,*) '# Class '// trim(class_name)
	else
		call write_message('# Class '// class_name)
	end if

	sub_para=D%subpara_class(class_name)
	call sub_para%dictionary%print(unit,.false.)

	if(present(unit))then
		write(unit,*)'#'
		write(unit,*)
	else
		call write_message('#')
		call write_message('')
	end if

end subroutine

subroutine read(G,unit)

	class(para_list),intent(inout)::G
	integer,intent(in)::unit
	character(len=max_char_length)::cur_class
	character(len=10*max_char_length)::line
	integer::io_stat,len,pos,i
	logical :: class_on
	character(len=max_char_length)::no_use,cur_name
	character(len=2)::type

	integer::ival 
	real(8)::dval 
	complex(8)::zval 
	character(len=max_char_length)::aval 
	logical::lval 
	integer,allocatable::mival(:)
	real(8),allocatable::mdval(:)
	character(len=max_char_length),allocatable::maval(:)
	logical,allocatable::mlval(:)

	call G%clean()

	cur_class='none'
	class_on=.false.
	read(unit,'(A)',IOSTAT=io_stat) line
	do while(.true.)
		if(io_stat/=0)then
			call wc_error_stop('para_list.read','EOF before identifier "/"')
		end if
		line=adjustl(line)
		type=''
		if(len_trim(line)>0) then
			pos=scan(line,' '//achar(9)) ! achar(9) for tab
			type=line(1:pos-1)
		end if
		select case(type)
		case('#')
			if(.not. class_on) then
				read(line(pos:),*) no_use,cur_class
				class_on=.true.
			else
				cur_class='none'
				class_on=.false.
			end if
		case('i')
			read(line(pos:),*) cur_name,ival
			call G%pinsert(cur_name,ival,cur_class)
		case('d')
			read(line(pos:),*) cur_name,dval
			call G%pinsert(cur_name,dval,cur_class)
		case('z')
			read(line(pos:),*) cur_name,zval
			call G%pinsert(cur_name,zval,cur_class)
		case('a')
			read(line(pos:),*) cur_name,aval
			call G%pinsert(cur_name,aval,cur_class)
		case('l')
			read(line(pos:),*) cur_name,lval
			call G%pinsert(cur_name,lval,cur_class)
		case('mi')
			read(line(pos:),*)cur_name,len
			if(allocated(mival)) deallocate(mival)
			allocate(mival(len))
			do i=1,len
				read(unit,*,IOSTAT=io_stat) mival(i)
				if(io_stat/=0) call wc_error_stop('para_list.read','array reading stopped')
			end do
			call G%pinsert(cur_name,mival,cur_class)
		case('md')
			read(line(pos:),*)cur_name,len
			if(allocated(mdval)) deallocate(mdval)
			allocate(mdval(len))
			do i=1,len
				read(unit,*,IOSTAT=io_stat) mdval(i)
				if(io_stat/=0) call wc_error_stop('para_list.read','array reading stopped')
			end do
			call G%pinsert(cur_name,mdval,cur_class)
		case('ma')
			read(line(pos:),*)cur_name,len
			if(allocated(maval)) deallocate(maval)
			allocate(maval(len))
			do i=1,len
				read(unit,*,IOSTAT=io_stat) maval(i)
				if(io_stat/=0) call wc_error_stop('para_list.read','array reading stopped')
			end do
			call G%pinsert(cur_name,maval,cur_class)
		case('ml')
			read(line(pos:),*)cur_name,len
			if(allocated(mlval)) deallocate(mlval)
			allocate(mlval(len))
			do i=1,len
				read(unit,*,IOSTAT=io_stat) mlval(i)
				if(io_stat/=0) call wc_error_stop('para_list.read','array reading stopped')
			end do
			call G%pinsert(cur_name,mlval,cur_class)
		case('/')
			exit
		end select

		read(unit,'(A)',IOSTAT=io_stat) line
	end do

end subroutine

subroutine append_para(P1,P2)

	class(para_list),intent(inout)::P1
	class(para_list),intent(in)::P2

	call P1%dictionary%append(P2%dictionary)
	call P1%dic_class%append(P2%dic_class)

end subroutine

end module 