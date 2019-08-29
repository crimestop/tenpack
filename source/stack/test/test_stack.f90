program main
use mod_stack
implicit none

	logical :: pass

	pass = test_push()
	pass = pass .and. test_pop()
	pass = pass .and. test_top()
	pass = pass .and. test_eq()
	pass = pass .and. test_copy()
	pass = pass .and. test_IO()

	if(pass) then
		write(*,*)'all pass'
	else
		write(*,*)'some failed'
	end if


contains

function test_push() result(pass)

	type(stack)::U
	logical :: pass

	write(*,*)'test push: '

	call U%push(10)
	pass = U%consistent()
	if(pass) pass = all (U%items()==[10])
	if( .not. pass)then
		write(*,*)'----- fail 1'
		return
	end if

	call U%push(10)
	call U%push(20)
	call U%push(30)
	pass = U%consistent()
	if(pass) pass = all (U%items()==[10,10,20,30])
	if( .not. pass)then
		write(*,*)'----- fail 2'
		return
	end if

	write(*,*)'----- pass'
	write(*,*)' '

end function

function test_pop() result(pass)

	type(stack)::U
	logical :: pass
	integer :: val

	write(*,*)'test pop: '

	call U%push(10)
	val = U%pop()
	pass = U%consistent()
	if(pass) pass = (val == 10)
	if(pass) pass = (U%num()==0)
	if( .not. pass)then
		write(*,*)'----- fail 1'
		return
	end if

	call U%push(10)
	call U%push(10)
	call U%push(20)
	call U%push(30)
	val = U%pop()
	pass = U%consistent()
	if(pass) pass = (val == 30)
	if(pass) pass = all (U%items()==[10,10,20])
	if(pass) val = U%pop()
	if(pass) pass = U%consistent()
	if(pass) pass = (val == 20)
	if(pass) pass = all (U%items()==[10,10])
	if(pass) val = U%pop()
	if(pass) pass = U%consistent()
	if(pass) pass = (val == 10)
	if(pass) pass = all (U%items()==[10])
	if(pass) val = U%pop()
	if(pass) pass = U%consistent()
	if(pass) pass = (val == 10)
	if(pass) pass = (U%num()==0)
	if( .not. pass)then
		write(*,*)'----- fail 2'
		return
	end if

	write(*,*)'----- pass'
	write(*,*)' '

end function

function test_top() result(pass)

	type(stack)::U
	logical :: pass
	integer :: val

	write(*,*)'test top: '

	call U%push(10)
	pass = (U%top() == 10)
	if(pass) call U%push(10)
	if(pass) pass = (U%top() == 10)
	if(pass) call U%push(20)
	if(pass) pass = (U%top() == 20)
	if(pass) call U%push(30)
	if(pass) pass = (U%top() == 30)
	if( .not. pass)then
		write(*,*)'----- fail 1'
		return
	end if

	val=U%pop()
	pass = (U%top() == 20)
	if(pass) val=U%pop()
	if(pass) pass = (U%top() == 10)
	if(pass) val=U%pop()
	if(pass) pass = (U%top() == 10)
	if( .not. pass)then
		write(*,*)'----- fail 2'
		return
	end if

	write(*,*)'----- pass'
	write(*,*)' '

end function

function test_eq() result(pass)

	type(stack)::U,V
	logical :: pass

	write(*,*)'test eq: '

	pass = (U==V)
	if( .not. pass)then
		write(*,*)'----- fail 1'
		return
	end if

	call U%push(10)
	pass = .not. (U==V)
	if( .not. pass)then
		write(*,*)'----- fail 2'
		return
	end if

	call V%push(10)
	pass = (U==V)
	if( .not. pass)then
		write(*,*)'----- fail 3'
		return
	end if

	call U%push(10)
	call U%push(20)
	call U%push(30)
	pass = .not. (U==V)
	if( .not. pass)then
		write(*,*)'----- fail 4'
		return
	end if

	call V%push(10)
	call V%push(20)
	call V%push(30)
	pass = (U==V)
	if( .not. pass)then
		write(*,*)'----- fail 5'
		return
	end if

	write(*,*)'----- pass'
	write(*,*)' '

end function

function test_copy() result(pass)

	type(stack)::U,V
	logical :: pass

	write(*,*)'test copy: '

	V=U
	pass = (U==V)
	if( .not. pass)then
		write(*,*)'----- fail 1'
		return
	end if

	call U%push(10)
	V=U
	pass = (U==V)
	if( .not. pass)then
		write(*,*)'----- fail 2'
		return
	end if

	call U%push(10)
	call U%push(20)
	call U%push(30)
	V=U
	pass = (U==V)
	if( .not. pass)then
		write(*,*)'----- fail 3'
		return
	end if

	write(*,*)'----- pass'
	write(*,*)' '

end function

function test_IO() result(pass)

	type(stack)::U,V
	logical :: pass

	write(*,*)'test I/O: '

	open(100,file='test.dat')
	call U%write(100)
	close(100)
	open(100,file='test.dat')
	call V%read(100)
	close(100)
	pass = (U==V)
	if( .not. pass)then
		write(*,*)'----- fail 1'
		return
	end if

	call U%push(1)
	open(100,file='test.dat')
	call U%write(100)
	close(100)
	open(100,file='test.dat')
	call V%read(100)
	close(100)
	pass = (U==V)
	if( .not. pass)then
		write(*,*)'----- fail 2'
		return
	end if

	call U%push(11)
	call U%push(22)
	call U%push(33)
	open(100,file='test.dat')
	call U%write(100)
	close(100)
	open(100,file='test.dat')
	call V%read(100)
	close(100)
	pass = (U==V)
	if( .not. pass)then
		write(*,*)'----- fail 3'
		return
	end if

	write(*,*)'----- pass'
	write(*,*)' '

end function

end program