MODULE tn_tensor_type
use error 
use tools
use tensor_network
use tensor_type
implicit none
private

type, extends(tensor):: tn_tensor
	private

	type(group)::grp

	contains
	private
	procedure,public:: get_info
	procedure,public:: draw
	procedure,public:: empty
	procedure,public:: absorb
	procedure,public:: take
	procedure:: absorb_all0
	procedure:: absorb_all1
	generic,public:: absorb_all=>absorb_all0,absorb_all1
	procedure,public:: belong
	procedure,public::absorb_except
	procedure,public::invert_bond

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

public tn_tensor,assignment(=),operator(*),operator(/),operator(.con.),absorb_rt,absorb_all_rt,TNcontract,&
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

	scale_tn%grp=T%grp

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

	divide_tn%grp=T%grp

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

	conjugate_tn%grp=T%grp
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
		T1%grp=T2%grp
		T1%tensor=T2%tensor
	end select

end subroutine

subroutine belong(T,L)

	class(tn_tensor),intent(inout)::T
	type(lattice),target,intent(in) ::L

	call T%grp%belong(L)

end subroutine

subroutine empty(T)

	class(tn_tensor),intent(inout)::T

	call T%grp%empty
	call T%tensor%empty()

end subroutine

subroutine draw(T,tnname,label_bond,fixed,check_tag)

	class(tn_tensor),intent(inout)::T
	character(len=*),intent(in)::tnname
	logical,intent(in),optional::label_bond,fixed,check_tag

	call T%grp%draw(tnname,tnname,label_bond,fixed,check_tag)

end subroutine

subroutine get_info(T)

	class(tn_tensor),intent(inout)::T
	integer::L1,L2

	call T%grp%get_info
	call writemess('The dim of tensor is :')
	call T%diminfo()
	call writemess('The program has been paused. Please press any key to continue')
	read(*,*)

end subroutine

subroutine take(T,pos)	!if already includes or pos have no tn, don't do anything

	class(tn_tensor),intent(inout)::T
	integer,intent(in)::pos(2)

	call T%grp%take(pos)
	if(draw_mode) call T%draw('tn_take')

end subroutine

subroutine absorb(T,pos)	!if already includes or pos have no tn, don't do anything

	class(tn_tensor),intent(inout)::T
	integer,intent(in)::pos(2)
		
	call lat_absorb_tensor(T%tensor,T%tensor,T%grp,pos)
	if(draw_mode) call T%draw('tn_absorb')

end subroutine

subroutine absorb_rt(Tout,Tin,pos)	!if already includes or pos have no tn, don't do anything

	class(tn_tensor),intent(inout)::Tout,Tin
	integer,intent(in)::pos(2)

	Tout%grp=Tin%grp
	call lat_absorb_tensor(Tout%tensor,Tin%tensor,Tout%grp,pos)
	if(draw_mode) call Tout%draw('tn_absorb')

end subroutine

type(tn_tensor) function contract_TNTN(T1,T2) result(Res)
	
	type(tn_tensor),intent(inout)::T1,T2
	integer::num

	Res%grp=T1%grp
	call lat_contract_type(Res%Tensor,T1%Tensor,T2%Tensor,Res%grp,T2%grp)
	if(draw_mode) call Res%draw('tn_contract')

end function

subroutine absorb_all1(T1,T2,in_dir)

	class(tn_tensor),intent(inout)::T1
	class(tn_tensor),intent(inout)::T2
	type(lattice),pointer::plat
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

	if(draw_mode) call T1%draw('tn_absorb_all_T1_before')
	call T1%grp%point_lat(plat)
	call plat%get_size(L1,L2)
	select case(dir)
	case('lu')
		do n=1,L2
			do m=1,L1
				if(.not. T2%grp%check_contain([m,n])) call T1%absorb([m,n])
			end do
		end do
	case('ru')
		do n=L2,1,-1
			do m=1,L1
				if(.not. T2%grp%check_contain([m,n])) call T1%absorb([m,n])
			end do
		end do
	case('ld')
		do n=1,L2
			do m=L1,1,-1
				if(.not. T2%grp%check_contain([m,n])) call T1%absorb([m,n])
			end do
		end do
	case('rd')
		do n=L2,1,-1
			do m=L1,1,-1
				if(.not. T2%grp%check_contain([m,n])) call T1%absorb([m,n])
			end do
		end do
	end select

	if(draw_mode) call T1%draw('tn_absorb_all_T1_after')
	if(draw_mode) call T2%draw('tn_absorb_all_T2')

	call lat_contract_type(T1%Tensor,T1%Tensor,T2%Tensor,T1%grp,T2%grp)
	if(draw_mode) call T1%draw('tn_absorb_result')

end subroutine

type(tn_tensor) function absorb_all1_rt(T1,T2,in_dir)result(Res)

	class(tn_tensor),intent(inout)::T1
	class(tn_tensor),intent(inout)::T2
	character(len=*),optional,intent(in)::in_dir

	Res=T1
	call Res%absorb_all1(T2,in_dir)

end function

subroutine absorb_all0(T,in_dir)

	class(tn_tensor),intent(inout)::T
	type(lattice),pointer::plat
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
	call T%grp%point_lat(plat)
	call plat%get_size(L1,L2)
	select case(dir)
	case('lu')
		do n=1,L2
			do m=1,L1
				call T%absorb([m,n])
			end do
		end do
	case('ru')
		do n=L2,1,-1
			do m=1,L1
				call T%absorb([m,n])
			end do
		end do
	case('ld')
		do n=1,L2
			do m=L1,1,-1
				call T%absorb([m,n])
			end do
		end do
	case('rd')
		do n=L2,1,-1
			do m=L1,1,-1
				call T%absorb([m,n])
			end do
		end do
	end select
	if(draw_mode) call T%draw('tn_absorb_all_result')

end subroutine

type(tn_tensor) function absorb_all0_rt(T,in_dir)result(Res)

	class(tn_tensor),intent(inout)::T
	character(len=*),optional,intent(in)::in_dir
	
	Res=T
	call Res%absorb_all0(in_dir)

end function

subroutine absorb_except(T,pos,in_dir)

	class(tn_tensor),intent(inout)::T
	type(lattice),pointer::plat
	integer,intent(in)::pos(2)
	character(len=*),optional,intent(in)::in_dir
	integer::L1,L2,m,n
	character(len=2)::dir

	call T%grp%point_lat(plat)

	if(.not.plat%check_exist(pos)) then
		call wc_error_stop('tn_tensor.absorb_except','site at pos does not exist')
	end if

	if(T%grp%check_contain(pos)) then
		call wc_error_stop('tn_tensor.absorb_except','site at pos already contained in the group')
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

	if(draw_mode) call T%draw('tn_absorb_all_except_T')
	call plat%get_size(L1,L2)
	select case(dir)
	case('lu')
		do n=1,L2
			do m=1,L1
				if(.not.(m==pos(1) .and. n==pos(2))) call T%absorb([m,n])
			end do
		end do
	case('ru')
		do n=L2,1,-1
			do m=1,L1
				if(.not.(m==pos(1) .and. n==pos(2))) call T%absorb([m,n])
			end do
		end do
	case('ld')
		do n=1,L2
			do m=L1,1,-1
				if(.not.(m==pos(1) .and. n==pos(2))) call T%absorb([m,n])
			end do
		end do
	case('rd')
		do n=L2,1,-1
			do m=L1,1,-1
				if(.not.(m==pos(1) .and. n==pos(2))) call T%absorb([m,n])
			end do
		end do
	end select
	if(draw_mode) call T%draw('tn_absorb_all_except_result')

end subroutine

subroutine invert_bond(T)

	class(tn_tensor),intent(inout)::T

	call T%grp%invert_bond(T%tensor)

end subroutine

end module tn_tensor_type
