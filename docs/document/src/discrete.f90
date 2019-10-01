module mod_discrete
!! (in libkernel)
!! the module to test if a variable has changed by a fixed amount
implicit none
private

type discreter
	private
	integer::lastval=0 
	integer::interval_type=0 ! 0 for int, 1 for dble
	integer::interval_int=1
	real(8)::interval_dbl=1d0
contains
	private
	procedure::set_interval_int,set_interval_dbl
	generic,public::set_interval=>set_interval_int,set_interval_dbl
	!! set the interval
	procedure::set_val_int,set_val_dbl
	generic,public::set_val=>set_val_int,set_val_dbl
	!! set last value
	procedure::test_int,test_dbl
	generic,public::test=>test_int,test_dbl
	!! test if value and last value differ by interval, set value -> last value
end type

public discreter

contains

subroutine set_interval_int(D,interval)
	!! set the integer interval

	class(discreter),intent(inout)::D
	integer,intent(in)::interval

	D%interval_type=0
	D%interval_int=interval

end subroutine

subroutine set_interval_dbl(D,interval)
	!! set the double interval

	class(discreter),intent(inout)::D
	real(8),intent(in)::interval

	D%interval_type=1
	D%interval_dbl=interval

end subroutine

subroutine set_val_int(D,val)
	!! set last integer value

	class(discreter),intent(inout)::D
	integer,intent(in)::val

	if(D%interval_type==0)then
		D%lastval=val/D%interval_int
	else
		D%lastval=val/D%interval_dbl
	end if

end subroutine

subroutine set_val_dbl(D,val)
	!! set last double value

	class(discreter),intent(inout)::D
	real(8),intent(in)::val

	if(D%interval_type==0)then
		D%lastval=val/D%interval_int
	else
		D%lastval=val/D%interval_dbl
	end if

end subroutine

function test_int(D,val) result(res)
	!! test if integer value and last value differ by interval, set integer value -> last value

	class(discreter),intent(inout)::D
	integer,intent(in)::val
	integer::new_val
	logical::res

	if(D%interval_type==0)then
		new_val=val/D%interval_int
	else
		new_val=val/D%interval_dbl
	end if
	res = (new_val/=D%lastval)
	if(res) D%lastval=new_val

end function

function test_dbl(D,val) result(res)
	!! test if double value and last value differ by interval, set double value -> last value

	class(discreter),intent(inout)::D
	real(8),intent(in)::val
	integer::new_val
	logical::res

	if(D%interval_type==0)then
		new_val=val/D%interval_int
	else
		new_val=val/D%interval_dbl
	end if
	res = (new_val/=D%lastval)
	if(res) D%lastval=new_val

end function

end module