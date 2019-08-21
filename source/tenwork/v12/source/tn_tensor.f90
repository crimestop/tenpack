MODULE tn_tensor_type
use error 
use tools
use tensor_network
use tensor_type
implicit none
private

type, extends(tensor):: tn_tensor
	private

	type(lattice),pointer::lat=>NULL()
	logical,allocatable::includes(:)

	contains
	private
	procedure,public:: get_info
	procedure,public:: draw
	procedure,public:: empty
	procedure,public:: absorb
	procedure,public:: take
	procedure,public:: absorb_all0
	procedure,public:: absorb_all1
	generic,public:: absorb_all=>absorb_all0,absorb_all1
	!procedure,public:: clean
	procedure,public:: belong
    !final :: delete_object=>clean
end type  


interface assignment(=)
	module procedure assignmentTN
end interface

interface operator(*)
	module procedure scale_tn
end interface

interface operator(/)
	module procedure divide_tn
end interface

interface operator(.con.)
	module procedure conjugate_tn
end interface

interface absorb_all_rt
	module procedure absorb_all0_rt
	module procedure absorb_all1_rt
end interface

interface TNcontract
	module procedure contract_TNTN
end interface

logical :: draw_mode=.false.

public tn_tensor,assignment(=),operator(*),operator(/),operator(.con.),absorb_rt,test_assignment,absorb_all_rt,TNcontract,&
	tn_draw_on,tn_draw_off

contains

subroutine tn_draw_on()
	draw_mode=.true.
end subroutine

subroutine tn_draw_off()
	draw_mode=.false.
end subroutine

type(tn_tensor) function scale_tn(T,mul)

	type(tn_tensor),intent(in)::T
	class(*),intent(in)::mul

	scale_tn%lat=>T%lat
	if(associated(T%lat))then
		allocate(scale_tn%includes(size(T%includes)))
		scale_tn%includes=T%includes
	end if

	select type(mul)
	type is(real(4))
		scale_tn%tensor=T%tensor*mul
	type is(real(8))
		scale_tn%tensor=T%tensor*mul
	type is(complex(4))
		scale_tn%tensor=T%tensor*mul
	type is(complex(8))
		scale_tn%tensor=T%tensor*mul
	end select

end function

type(tn_tensor) function divide_tn(T,mul)

	type(tn_tensor),intent(in)::T
	class(*),intent(in)::mul

	divide_tn%lat=>T%lat
	if(associated(T%lat))then
		allocate(divide_tn%includes(size(T%includes)))
		divide_tn%includes=T%includes
	end if

	select type(mul)
	type is(real(4))
		divide_tn%tensor=T%tensor/mul
	type is(real(8))
		divide_tn%tensor=T%tensor/mul
	type is(complex(4))
		divide_tn%tensor=T%tensor/mul
	type is(complex(8))
		divide_tn%tensor=T%tensor/mul
	end select

end function

type(tn_tensor) function conjugate_tn(T)

	type(tn_tensor),intent(in)::T

	conjugate_tn%lat=>T%lat
	if(associated(T%lat))then
		allocate(conjugate_tn%includes(size(T%includes)))
		conjugate_tn%includes=T%includes
	end if

	conjugate_tn%tensor=.con. T%tensor	

end function

subroutine assignmentTN(T1,T2)

	type(tn_tensor),intent(in)::T2
	class(*),intent(inout)::T1

	select type(T1)
	type is(real(4))
		if(T2%gettotaldata()==1) then
			T1=T2%si([1,1])
		else
			call wc_error_stop('assignmentTN','Tensor should be 1D to assign to a real4 number')
		end if
	type is(real(8))
		if(T2%gettotaldata()==1) then
			T1=T2%di([1,1])
		else
			call wc_error_stop('assignmentTN','Tensor should be 1D to assign to a real8 number')
		end if
	type is(complex(4))
		if(T2%gettotaldata()==1) then
			T1=T2%ci([1,1])
		else
			call wc_error_stop('assignmentTN','Tensor should be 1D to assign to a com4 number')
		end if
	type is(complex(8))
		if(T2%gettotaldata()==1) then
			T1=T2%zi([1,1])
		else
			call wc_error_stop('assignmentTN','Tensor should be 1D to assign to a com8 number')
		end if
	type is(tensor)
		T1=T2%tensor
	type is(tn_tensor)
		T1%lat=>T2%lat
		if(associated(T1%lat))then
			if(allocated(T1%includes))then
				if(size(T1%includes)/=size(T2%includes)) then
					deallocate(T1%includes)
					allocate(T1%includes(size(T2%includes)))
				end if
			else
				allocate(T1%includes(size(T2%includes)))
			end if
			T1%includes=T2%includes
		end if
		T1%tensor=T2%tensor
	end select

end subroutine

subroutine test_assignment(T1,T2)

	type(tn_tensor),intent(in)::T2
	type(tn_tensor),intent(inout)::T1

	call writemess('in')

	T1=T2
	call writemess('out')
	write(*,'(L2)') T1%includes
	write(*,'(L2)') T2%includes
	write(*,*)associated(T1%lat, T2%lat)
	return

end subroutine

subroutine belong(T,L)

	class(tn_tensor),intent(inout)::T
	type(lattice),target,intent(in) ::L
	integer::num

	T%lat=>L

	num=max(L%get_max_site_num(),1)
	if(allocated(T%includes))then
		if(size(T%includes)/=num) then
			deallocate(T%includes)
			allocate(T%includes(num))
		end if
	else
		allocate(T%includes(num))
	end if
	T%includes=.false.
	if(draw_mode) call T%draw('tn_belong')

end subroutine

subroutine empty(T)
implicit none

	class(tn_tensor),intent(inout)::T

	if(allocated(T%includes)) T%includes=.false.
	call T%tensor%empty()

end subroutine

subroutine draw(T,tnname,label_bond,fixed)
implicit none

	class(tn_tensor),intent(inout)::T
	character(len=*),intent(in)::tnname
	logical,intent(in),optional::label_bond,fixed

	if(.not.associated(T%lat)) then
		call writemess('-------------------------------------------------------')
		call writemess('tensor not belong to any lattice, do not draw anything')
		call writemess('-------------------------------------------------------')
		return
	end if
	call T%lat%draw(tnname,tnname,T%includes,label_bond,fixed)

end subroutine

subroutine get_info(T)
implicit none

	class(tn_tensor),intent(inout)::T
	integer::L1,L2

	call T%lat%get_size(L1,L2)
	call writemess('This tensor belongs to the lattice :'+T%lat%get_name())
	call writemess('with tensors included are')
	write(*,'(L2)') T%includes
	call writemess('Its dimension info is :')
	call T%diminfo()
	call writemess('The program has been paused. Please press any key to continue')
	read(*,*)

end subroutine

subroutine take(T,pos)	!if already includes or pos have no tn, don't do anything

	class(tn_tensor),intent(inout)::T
	integer,intent(in)::pos(2)

	if(T%lat%check_exist(pos))then
		T%includes(T%lat%get_rawpos(pos))=.true.
	end if
	if(draw_mode) call T%draw('tn_take')

end subroutine

subroutine absorb(T,pos)	!if already includes or pos have no tn, don't do anything

	class(tn_tensor),intent(inout)::T
	integer,intent(in)::pos(2)
		
	call T%lat%absorb_tensor(T%tensor,T%tensor,pos,T%includes)
	if(draw_mode) call T%draw('tn_absorb')

end subroutine

subroutine absorb_rt(Tout,Tin,pos)	!if already includes or pos have no tn, don't do anything

	class(tn_tensor),intent(inout)::Tout,Tin
	integer,intent(in)::pos(2)

	if (.not.associated(Tout%lat, Tin%lat)) then
		call Tout%belong(Tin%lat)
	end if
	Tout%includes=Tin%includes
		
	call Tout%lat%absorb_tensor(Tout%tensor,Tin%tensor,pos,Tout%includes)
	if(draw_mode) call Tout%draw('tn_absorb')

end subroutine

type(tn_tensor) function contract_TNTN(T1,T2) result(Res)
	
	type(tn_tensor),intent(inout)::T1,T2
	integer::num

	if (.not.associated(T1%lat, T2%lat)) then
		call wc_error_stop('contract_TNTN','Two tensors should not belong to different lattices.')
	end if
	if (any(T1%includes.and.T2%includes)) then
		call wc_error_stop('contract_TNTN','Two tensors should not overlap.')
	end if

	call T1%lat%contract_type(Res%Tensor,T1%Tensor,T2%Tensor,T1%includes,T2%includes)
	Res%lat=>T1%lat
	num=max(Res%lat%get_max_site_num(),1)
	allocate(Res%includes(num))
	Res%includes=T1%includes.or.T2%includes
	if(draw_mode) call Res%draw('tn_contract')

end function


subroutine absorb_all1(T1,T2,in_dir)

	class(tn_tensor),intent(inout)::T1
	class(tn_tensor),intent(inout)::T2
	character(len=*),optional,intent(in)::in_dir
	integer::L1,L2,m,n
	character(len=2)::dir

	if (.not.associated(T1%lat, T2%lat)) then
		call wc_error_stop('absorb_all1','Two tensors should not belong to different lattices.')
	end if

	if(present(in_dir))then
		select case(in_dir)
		case('l')
			dir='lu'
		case('r')
			dir='ru'
		case('lu')
			dir='lu'
		case('ru')
			dir='ru'
		case('ld')
			dir='ld'
		case('rd')
			dir='rd'
		case default
			dir='lu'
		end select
	else
		dir='lu'
	end if

	if(draw_mode) call T1%draw('tn_absorb_all_T1_before')
	call T1%lat%get_size(L1,L2)
	select case(dir)
	case('lu')
		do n=1,L2
			do m=1,L1
				call T1%lat%absorb_tensor(T1%tensor,T1%tensor,[m,n],T1%includes,T2%includes)
			end do
		end do
	case('ru')
		do n=L2,1,-1
			do m=1,L1
				call T1%lat%absorb_tensor(T1%tensor,T1%tensor,[m,n],T1%includes,T2%includes)
			end do
		end do
	case('ld')
		do n=1,L2
			do m=L1,1,-1
				call T1%lat%absorb_tensor(T1%tensor,T1%tensor,[m,n],T1%includes,T2%includes)
			end do
		end do
	case('rd')
		do n=L2,1,-1
			do m=L1,1,-1
				call T1%lat%absorb_tensor(T1%tensor,T1%tensor,[m,n],T1%includes,T2%includes)
			end do
		end do
	end select

	if(draw_mode) call T1%draw('tn_absorb_all_T1_after')
	if(draw_mode) call T2%draw('tn_absorb_all_T2')

	call T1%lat%contract_type(T1%Tensor,T1%Tensor,T2%Tensor,T1%includes,T2%includes)
	T1%includes=.true.
	if(draw_mode) call T1%draw('tn_absorb_result')

end subroutine

type(tn_tensor) function absorb_all1_rt(T1,T2,in_dir)result(Res)
implicit none

	class(tn_tensor),intent(inout)::T1
	class(tn_tensor),intent(inout)::T2
	character(len=*),optional,intent(in)::in_dir

	Res=T1
	call Res%absorb_all1(T2,in_dir)

end function


subroutine absorb_all0(T,in_dir)
implicit none

	class(tn_tensor),intent(inout)::T
	character(len=*),optional,intent(in)::in_dir
	integer::L1,L2,m,n
	character(len=2)::dir

	if(present(in_dir))then
		select case(in_dir)
		case('l')
			dir='lu'
		case('r')
			dir='ru'
		case('lu')
			dir='lu'
		case('ru')
			dir='ru'
		case('ld')
			dir='ld'
		case('rd')
			dir='rd'
		case default
			dir='lu'
		end select
	else
		dir='lu'
	end if

	if(draw_mode) call T%draw('tn_absorb_all_T')

	call T%lat%get_size(L1,L2)
	select case(dir)
	case('lu')
		do n=1,L2
			do m=1,L1
				call T%lat%absorb_tensor(T%tensor,T%tensor,[m,n],T%includes)
			end do
		end do
	case('ru')
		do n=L2,1,-1
			do m=1,L1
				call T%lat%absorb_tensor(T%tensor,T%tensor,[m,n],T%includes)
			end do
		end do
	case('ld')
		do n=1,L2
			do m=L1,1,-1
				call T%lat%absorb_tensor(T%tensor,T%tensor,[m,n],T%includes)
			end do
		end do
	case('rd')
		do n=L2,1,-1
			do m=L1,1,-1
				call T%lat%absorb_tensor(T%tensor,T%tensor,[m,n],T%includes)
			end do
		end do
	end select
	T%includes=.true.
	if(draw_mode) call T%draw('tn_absorb_all_result')

end subroutine

type(tn_tensor) function absorb_all0_rt(T,in_dir)result(Res)
implicit none

	class(tn_tensor),intent(inout)::T
	character(len=*),optional,intent(in)::in_dir
	
	Res=T
	call Res%absorb_all0(in_dir)

end function

end module tn_tensor_type
