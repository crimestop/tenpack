MODULE tn_tensor_type
use error 
use usefull_function
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
		procedure,public:: absorb_all0
		procedure,public:: absorb_all1
		generic,public:: absorb_all=>absorb_all0,absorb_all1
		!procedure,public:: clean
		procedure,public:: belong
	    !final :: delete_object=>clean
	end type  

	interface assignment(=)
		module procedure assignmentTNTN!T1=T2, both T1 and T2 are TNTensor
		module procedure assignmentTTN !T1=T2, T1 is Tensor and T2 is TNTensor
		module procedure assignmentcom8TN 
		module procedure assignmentcom4TN 
		module procedure assignmentreal8TN 
		module procedure assignmentreal4TN 
	end interface

	interface operator(*)
		module procedure scale_tn_dt
		module procedure scale_tn_st
		module procedure scale_tn_ct
		module procedure scale_tn_zt
		module procedure scale_tn_td
		module procedure scale_tn_tz
		module procedure scale_tn_ts
		module procedure scale_tn_tc
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

	public tn_tensor,assignment(=),operator(*),operator(/),operator(.con.),absorb_rt,test_assignment,absorb_all_rt,TNcontract

contains

function scale_tn_td(T,mul) result(Tout)
implicit none

	type(tn_tensor),intent(in)::T
	type(tn_tensor)::Tout
	real(8),intent(in)::mul

	Tout%lat=>T%lat
	if(associated(T%lat))then
		allocate(Tout%includes(size(T%includes)))
		Tout%includes=T%includes
	end if

	Tout%tensor=T%tensor*mul

end function

function scale_tn_ts(T,mul) result(Tout)
implicit none

	type(tn_tensor),intent(in)::T
	type(tn_tensor)::Tout
	real(4),intent(in)::mul

	Tout%lat=>T%lat
	if(associated(T%lat))then
		allocate(Tout%includes(size(T%includes)))
		Tout%includes=T%includes
	end if

	Tout%tensor=T%tensor*mul

end function

function scale_tn_tz(T,mul) result(Tout)
implicit none

	type(tn_tensor),intent(in)::T
	type(tn_tensor)::Tout
	complex(8),intent(in)::mul

	Tout%lat=>T%lat
	if(associated(T%lat))then
		allocate(Tout%includes(size(T%includes)))
		Tout%includes=T%includes
	end if

	Tout%tensor=T%tensor*mul

end function

function scale_tn_tc(T,mul) result(Tout)
implicit none

	type(tn_tensor),intent(in)::T
	type(tn_tensor)::Tout
	complex(4),intent(in)::mul

	Tout%lat=>T%lat
	if(associated(T%lat))then
		allocate(Tout%includes(size(T%includes)))
		Tout%includes=T%includes
	end if

	Tout%tensor=T%tensor*mul

end function

function scale_tn_st(mul,T) result(Tout)
implicit none

	type(tn_tensor),intent(in)::T
	type(tn_tensor)::Tout
	real(4),intent(in)::mul

	Tout%lat=>T%lat
	if(associated(T%lat))then
		allocate(Tout%includes(size(T%includes)))
		Tout%includes=T%includes
	end if

	Tout%tensor=T%tensor*mul

end function

function scale_tn_dt(mul,T) result(Tout)
implicit none

	type(tn_tensor),intent(in)::T
	type(tn_tensor)::Tout
	real(8),intent(in)::mul

	Tout%lat=>T%lat
	if(associated(T%lat))then
		allocate(Tout%includes(size(T%includes)))
		Tout%includes=T%includes
	end if

	Tout%tensor=T%tensor*mul

end function

function scale_tn_ct(mul,T) result(Tout)
implicit none

	type(tn_tensor),intent(in)::T
	type(tn_tensor)::Tout
	complex(4),intent(in)::mul

	Tout%lat=>T%lat
	if(associated(T%lat))then
		allocate(Tout%includes(size(T%includes)))
		Tout%includes=T%includes
	end if

	Tout%tensor=T%tensor*mul

end function

function scale_tn_zt(mul,T) result(Tout)
implicit none

	type(tn_tensor),intent(in)::T
	type(tn_tensor)::Tout
	complex(8),intent(in)::mul

	Tout%lat=>T%lat
	if(associated(T%lat))then
		allocate(Tout%includes(size(T%includes)))
		Tout%includes=T%includes
	end if

	Tout%tensor=T%tensor*mul

end function


type(tn_tensor) function divide_tn(T,mul)
implicit none

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
implicit none

	type(tn_tensor),intent(in)::T

	conjugate_tn%lat=>T%lat
	if(associated(T%lat))then
		allocate(conjugate_tn%includes(size(T%includes)))
		conjugate_tn%includes=T%includes
	end if

	conjugate_tn%tensor=.con. T%tensor	

end function

subroutine assignmentTNTN(T1,T2)
implicit none

	type(tn_tensor),intent(in)::T2
	type(tn_tensor),intent(inout)::T1

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

end subroutine


subroutine test_assignment(T1,T2)
implicit none

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

subroutine assignmentTTN(T1,T2)
implicit none

	type(tn_tensor),intent(in)::T2
	type(tensor),intent(inout)::T1

	T1=T2%tensor
	return

end subroutine

subroutine assignmentcom8TN(outn,T)
implicit none

	type(tn_tensor),intent(in)::T
	complex(kind=8),intent(out)::outn

	if(T%gettotaldata()==1) then
		outn=T%zi([1,1])
	else
		call wc_error_stop('assignmentcom8TN','Tensor should be 1D to assign to a number')
	end if
	return

end subroutine

subroutine assignmentcom4TN(outn,T)
implicit none

	type(tn_tensor),intent(in)::T
	complex(kind=4),intent(out)::outn

	if(T%gettotaldata()==1) then
		outn=T%ci([1,1])
	else
		call wc_error_stop('assignmentcom4TN','Tensor should be 1D to assign to a number')
	end if
	return

end subroutine

subroutine assignmentreal8TN(outn,T)
implicit none

	type(tn_tensor),intent(in)::T
	real(kind=8),intent(out)::outn

	if(T%gettotaldata()==1) then
		outn=T%di([1,1])
	else
		call wc_error_stop('assignmentreal8TN','Tensor should be 1D to assign to a number')
	end if
	return

end subroutine

subroutine assignmentreal4TN(outn,T)
implicit none

	type(tn_tensor),intent(in)::T
	real(kind=4),intent(out)::outn

	if(T%gettotaldata()==1) then
		outn=T%zi([1,1])
	else
		call wc_error_stop('assignmentreal4TN','Tensor should be 1D to assign to a number')
	end if
	return

end subroutine

subroutine belong(T,L)
implicit none

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

subroutine absorb(T,pos)	!if already includes or pos have no tn, don't do anything
implicit none

	class(tn_tensor),intent(inout)::T
	integer,intent(in)::pos(2)
		
	call T%lat%absorb_tensor(T%tensor,T%tensor,pos,T%includes)

end subroutine

subroutine absorb_rt(Tout,Tin,pos)	!if already includes or pos have no tn, don't do anything
implicit none

	class(tn_tensor),intent(inout)::Tout,Tin
	integer,intent(in)::pos(2)

	if (.not.associated(Tout%lat, Tin%lat)) then
		call Tout%belong(Tin%lat)
	end if
	Tout%includes=Tin%includes
		
	call Tout%lat%absorb_tensor(Tout%tensor,Tin%tensor,pos,Tout%includes)

end subroutine

type(tn_tensor) function contract_TNTN(T1,T2)result(Res)
implicit none
	
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

end function


subroutine absorb_all1(T1,T2,in_dir)
implicit none

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

	call T1%lat%contract_type(T1%Tensor,T1%Tensor,T2%Tensor,T1%includes,T2%includes)

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

	!call T%draw('test','phi','n')

	call T%lat%get_size(L1,L2)
	select case(dir)
	case('lu')
		do n=1,L2
			do m=1,L1
				!write(*,*)n,m
				call T%lat%absorb_tensor(T%tensor,T%tensor,[m,n],T%includes)
				!call T%print()
				!call T%draw('test','phi','n')
			end do
		end do
		!call T%print()
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

end subroutine

type(tn_tensor) function absorb_all0_rt(T,in_dir)result(Res)
implicit none

	class(tn_tensor),intent(inout)::T
	character(len=*),optional,intent(in)::in_dir
	
	Res=T
	call Res%absorb_all0(in_dir)

end function

end module tn_tensor_type
