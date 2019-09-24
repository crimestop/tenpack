module mod_stack
use error
implicit none
private

type node
	type(node),pointer::previous
	integer::val
end type

type stack
	private
	integer::stack_num=0
	type(node),pointer::last=>null()

	contains
	procedure,public::push
	procedure,public::pop
	procedure,public::top
	procedure,public::num
	procedure,public::clean
	procedure,public::read
	procedure,public::write
	procedure::real_length
	procedure,public::consistent
	procedure,public::items ! public only for test, don't use
	procedure::copy
	generic,public :: assignment(=) => copy
	procedure::compare
	generic,public :: operator(==) => compare
end type

public stack

contains

subroutine push(S,val)

	class(stack) :: S 
	integer,intent(in) :: val
	type(node),pointer :: new

	S%stack_num=S%stack_num+1
	allocate(new)
	new%val=val
	new%previous=>S%last
	S%last=>new
	
end subroutine

function pop(S) result(res)

	class(stack) :: S 
	integer :: res
	type(node),pointer :: old

	if(S%stack_num==0)then
		res=-1
		call wc_error_stop('stack.pop','stack is empty')
	else
		S%stack_num=S%stack_num-1
		res=S%last%val
		old=>S%last
		S%last=>old%previous
		deallocate(old)
	end if
	
end function

function top(S) result(res)

	class(stack) :: S 
	integer :: res

	if(S%stack_num==0)then
		res=-1
		call wc_error_stop('stack.pop','stack is empty')
	else
		res=S%last%val
	end if
	
end function

function num(S) result(res)

	class(stack) :: S 
	integer::res

	res=S%stack_num

end function

subroutine clean(S)

	class(stack) :: S 
	type(node),pointer :: old

	do while(S%stack_num>0)
		S%stack_num=S%stack_num-1
		old=>S%last
		S%last=>old%previous
		deallocate(old)
	end do
	
end subroutine

subroutine write(S,unit)

	class(stack) :: S
	integer,intent(in) :: unit

	write(unit,*) S%stack_num
	if(S%stack_num>0) write(unit,*) S%items()

end subroutine

subroutine read(S,unit)

	class(stack) :: S
	integer,intent(in) :: unit
	integer :: i,num
	integer,allocatable :: vals(:)

	call S%clean
	read(unit,*) num
	if(num>0)then
		allocate(vals(num))
		read(unit,*) vals
		do i=1,num
			call S%push(vals(i))
		end do
	end if
	
end subroutine

subroutine copy(Sout,Sin)

	class(stack),intent(inout) :: Sout
	class(stack),intent(in) :: Sin
	integer :: i,num
	integer,allocatable :: inverse(:)
	type(node),pointer :: pnode

	num = Sin%stack_num

	allocate(inverse(num))
	pnode=>Sin%last

	do i=1,num
		inverse(i)=pnode%val
		pnode=>pnode%previous
	end do

	call Sout%clean
	do i=num,1,-1
		call Sout%push(inverse(i))
	end do
	
end subroutine

function compare(S1,S2) result(res)

	class(stack),intent(in)::S1,S2
	logical :: res

	res=S1%consistent() .and. S2%consistent() .and. (S1%stack_num == S2%stack_num)
	if(res) res=res .and. all(S1%items() == S2%items())

end function

function real_length(S) result(res)

	class(stack),intent(in)::S
	integer :: res
	type(node),pointer :: pnode

	res=0
	pnode=>S%last
	do while(associated(pnode))
		res=res+1
		pnode=>pnode%previous
	end do

end function

function consistent(S) result(res)

	class(stack),intent(in)::S
	logical :: res

	res=(S%stack_num == S%real_length())

end function

function items(S) result(res)

	class(stack),intent(in)::S
	integer,allocatable :: res(:)
	integer::i,num
	type(node),pointer :: pnode

	num=S%real_length()
	allocate(res(num))
	pnode=>S%last
	do i=num,1,-1
		res(i)=pnode%val
		pnode=>pnode%previous
	end do

end function

end module