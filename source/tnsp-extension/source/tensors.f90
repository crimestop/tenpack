module tensor_types
use Tensor_type
use SymTensor_type
use fermiTensor
use QuantumNumber_Type 
use Tools
use error
use string
implicit none

private
type tensors

	private
	type(Tensor)::T
	type(SymTensor)::symT
	type(fTensor)::fT
	character(len=20)::type='tensor'

contains

	private
	procedure,public::set_type
	procedure,public::print
	procedure,public::write
	procedure,public::read
	procedure::allocate1char,allocate1int,allocate2char,allocate2int
	generic,public::allocate=>allocate1char,allocate1int,allocate2char,allocate2int
	procedure,public::random
	procedure,public::zero
	procedure,public::gettype
	procedure::eyeTs
	generic,public::eye=>eyeTs
	procedure::setnamechrchr,setnameintchr
	generic,public::setname=>setnamechrchr,setnameintchr
	procedure::contract_char,contract_chars
	procedure::contract2_chars
	generic,public::contract=>contract_char,contract_chars,contract2_chars
	procedure,public::inverse
	procedure::sqrtTs
	generic,public::sqrt=>sqrtTs
	procedure,public::deallocate
	procedure,public::getflag
	procedure,public::nameorder
	procedure,public::diminfo
	procedure,public::empty
	procedure,public::getrank
	procedure,public::fuse
	procedure,public::split
	procedure,public::permute
	procedure,public::settype
	procedure,public::static
	procedure,public::dnorm
	procedure::dmaxTs
	generic,public::dmax=>dmaxTs
	procedure::getname_i,getname_all
	generic,public::getname=>getname_i,getname_all
	procedure,public::backward
	procedure,public::subtensor
	procedure,public::LQ
	procedure,public::QR
	procedure,public::SVD
	procedure::dimTs
	generic,public::dim=>dimTs
	procedure,public::outname
	procedure,public::outtensorname
	procedure,public::get_diag
	procedure,public::emptyZeroBlock

end type

interface assignment(=)
	module procedure assignmentanyTs
	module procedure assignmentTsT
	module procedure assignmentTssymT
	module procedure assignmentTsfT
end interface

interface operator(.kron.)
	module procedure directProduct
end interface
interface operator(.xx.)
	module procedure directProduct2
end interface
interface operator(+)
	module procedure addition
end interface
interface operator(-)
	module procedure minus
end interface
interface operator(*)
	module procedure scale_ts
end interface
interface operator(/)
	module procedure divide_ts
end interface
interface operator(.con.)
	module procedure conjugate
end interface
interface transpose
	module procedure transpose_Ts
end interface

interface contract
	module procedure contract_routine_nochar
	module procedure contract_routine_char
	module procedure contract_routine_chars
end interface

interface expm
	module procedure  expm_ts
end interface

public::tensors,square_mat,operator(.kron.),operator(.xx.),operator(+),operator(-),contract,assignment(=),&
		operator(*),operator(/),operator(.con.),expm,dbl,MPI_BCAST_Tensors,transpose
contains

subroutine set_type(Ts,type)

	class(tensors),intent(inout)::Ts
	character(len=*),intent(in)::type

	select case(type)
	case('tensor','symtensor','fermitensor')
		Ts%type=type
	case default
		call wc_error_stop('tensors.set_type','Unsuportted type: '//trim(type))
	end select

end subroutine

subroutine deallocate(Ts)

	class(tensors),intent(inout)::Ts

	select case(Ts%type)
	case('tensor')
		call Ts%T%deallocate()
	case('symtensor')
		call Ts%symT%deallocate()
	case('fermitensor')
		call Ts%fT%deallocate()
	case default
		call wc_error_stop('tensors.deallocate','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine empty(Ts)

	class(tensors),intent(inout)::Ts

	select case(Ts%type)
	case('tensor')
		call Ts%T%empty()
	case('symtensor')
		call Ts%symT%empty()
	case('fermitensor')
		call Ts%fT%empty()
	case default
		call wc_error_stop('tensors.empty','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine write(Ts,unit)

	class(tensors),intent(in)::Ts
	integer,optional,intent(in)::unit

	select case(Ts%type)
	case('tensor')
		if(present(unit)) then
			call Ts%T%write(unit)
		else
			call Ts%T%write(6)
		end if
	case('symtensor')
		if(present(unit)) then
			call Ts%symT%write(unit)
		else
			call Ts%symT%write(6)
		end if
	case('fermitensor')
		if(present(unit)) then
			call Ts%fT%write(unit)
		else
			call Ts%fT%write(6)
		end if
	case default
		call wc_error_stop('tensors.set_type','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine print(Ts)

	class(tensors),intent(in)::Ts

	call writemess('type is '//trim(Ts%type))
	select case(Ts%type)
	case('tensor')
		call Ts%T%print()
	case('symtensor')
		call Ts%symT%print()
	case('fermitensor')
		call Ts%fT%print()
	case default
		call wc_error_stop('tensors.set_type','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine read(Ts,unit)

	class(tensors),intent(inout)::Ts
	integer,intent(in)::unit

	select case(Ts%type)
	case('tensor')
		call Ts%T%read(unit)
	case('symtensor')
		call Ts%symT%read(unit)
	case('fermitensor')
		call Ts%fT%read(unit)
	case default
		call wc_error_stop('tensors.set_type','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine allocate1char(Ts,shape,datatype)

	class(tensors),intent(inout)::Ts
	integer,intent(in)::shape(:)
	character(len=*),intent(in)::datatype

	select case(Ts%type)
	case('tensor')
		call Ts%T%allocate(shape,datatype)
	case('symtensor','fermitensor')
		call wc_error_stop('tensors.allocate','Incorrect allocate method for '//trim(Ts%type))
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine allocate1int(Ts,shape,datatype)

	class(tensors),intent(inout)::Ts
	integer,intent(in)::shape(:)
	integer,intent(in)::datatype

	select case(Ts%type)
	case('tensor')
		call Ts%T%allocate(shape,datatype)
	case('symtensor','fermitensor')
		call wc_error_stop('tensors.allocate','Incorrect allocate method for '//trim(Ts%type))
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine allocate2char(Ts,symshape,datatype)

	class(tensors),intent(inout)::Ts
	type(QuanNum),intent(in)::symshape(:)
	character(len=*),intent(in)::datatype

	select case(Ts%type)
	case('tensor')
		call wc_error_stop('tensors.allocate','Incorrect allocate method for '//trim(Ts%type))
	case('symtensor')
		call Ts%symT%allocate(symshape,datatype)
	case('fermitensor')
		call Ts%fT%allocate(symshape,datatype)
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine allocate2int(Ts,symshape,datatype)

	class(tensors),intent(inout)::Ts
	type(QuanNum),intent(in)::symshape(:)
	integer,intent(in)::datatype

	select case(Ts%type)
	case('tensor')
		call wc_error_stop('tensors.allocate','Incorrect allocate method for '//trim(Ts%type))
	case('symtensor')
		call Ts%symT%allocate(symshape,datatype)
	case('fermitensor')
		call Ts%fT%allocate(symshape,datatype)
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine random(Ts,region)

	class(tensors),intent(inout)::Ts
	real(8),optional,intent(in)::region(2)

	select case(Ts%type)
	case('tensor')
		call Ts%T%random(region)
	case('symtensor')
		call Ts%symT%symrandom(region)
	case('fermitensor')
		call Ts%fT%symrandom(region)
	case default
		call wc_error_stop('tensors.random','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

function square_mat(Ts,name) result(Sq)

	type(tensors),intent(inout)::Ts
	character(len=*),intent(in)::name
	type(tensors)::Sq
	integer::D
	type(QuanNum)::QN,ivQN

	call Sq%set_type(Ts%type)
	select case(Ts%type)
	case('tensor')
		D=Ts%T%dim(name)
		call Sq%allocate([D,D],Ts%T%gettype())
	case('symtensor')		! 1st ind: inv rule of Ts, 2nd ind : rule of Ts
		QN=Ts%symT%quantumnumber(name)
		ivQN=QN
		call ivQN%setrule(-QN%getrule())
		call Sq%allocate([ivQN,QN],Ts%symT%gettype())
	case('fermitensor')
		QN=Ts%fT%quantumnumber(name)
		call Sq%allocate([QN,QN],Ts%fT%gettype())
	case default
		call wc_error_stop('tensors.square_mat','Unsuportted type: '//trim(Ts%type))
	end select

end function

subroutine eyeTs(Ts)

	class(tensors),intent(inout)::Ts

	select case(Ts%type)
	case('tensor')
		call Ts%T%eye()
	case('symtensor')
		call Ts%symT%eye()
	case('fermitensor')
		call Ts%fT%eye()
	case default
		call wc_error_stop('tensors.eye','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine zero(Ts)

	class(tensors),intent(inout)::Ts

	select case(Ts%type)
	case('tensor')
		call Ts%T%zero()
	case('symtensor')
		call Ts%symT%zero()
	case('fermitensor')
		call Ts%fT%zero()
	case default
		call wc_error_stop('tensors.setvalue','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

function gettype(Ts)result(type_int)

	class(tensors),intent(inout)::Ts
	integer::type_int

	select case(Ts%type)
	case('tensor')
		type_int=Ts%T%gettype()
	case('symtensor')
		type_int=Ts%symT%gettype()
	case('fermitensor')
		type_int=Ts%fT%gettype()
	case default
		call wc_error_stop('tensors.gettype','Unsuportted type: '//trim(Ts%type))
	end select

end function

subroutine setnameintchr(Ts,pos,name)

	class(tensors),intent(inout)::Ts
	integer,intent(in)::pos
	character(len=*),intent(in)::name

	select case(Ts%type)
	case('tensor')
		call Ts%T%setname(pos,name)
	case('symtensor')
		call Ts%symT%setname(pos,name)
	case('fermitensor')
		call Ts%fT%setname(pos,name)
	case default
		call wc_error_stop('tensors.setname','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine setnamechrchr(Ts,name1,name2)

	class(tensors),intent(inout)::Ts
	character(len=*),intent(in)::name1,name2

	select case(Ts%type)
	case('tensor')
		call Ts%T%setname(name1,name2)
	case('symtensor')
		call Ts%symT%setname(name1,name2)
	case('fermitensor')
		call Ts%fT%setname(name1,name2)
	case default
		call wc_error_stop('tensors.setname','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

function getflag(Ts) result(flag)

	class(tensors),intent(inout)::Ts
	logical::flag

	select case(Ts%type)
	case('tensor')
		flag=Ts%T%getflag()
	case('symtensor')
		flag=Ts%symT%getflag()
	case('fermitensor')
		flag=Ts%fT%getflag()
	case default
		call wc_error_stop('tensors.getflag','Unsuportted type: '//trim(Ts%type))
	end select

end function

function nameorder(Ts,ind) result(ord)

	class(tensors),intent(inout)::Ts
	character(len=*),intent(in)::ind
	type(tensor)::ord
	logical::flag

	select case(Ts%type)
	case('tensor')
		ord=Ts%T%nameorder(ind)
	case('symtensor')
		ord=Ts%symT%nameorder(ind)
	case('fermitensor')
		ord=Ts%fT%nameorder(ind)
	case default
		call wc_error_stop('tensors.nameorder','Unsuportted type: '//trim(Ts%type))
	end select

end function

subroutine emptyZeroBlock(Ts)

	class(tensors),intent(inout)::Ts

	select case(Ts%type)
	case('tensor')
	case('symtensor')
		call Ts%symT%emptyZeroBlock()

	case('fermitensor')
		call Ts%fT%emptyZeroBlock()
	case default
		call wc_error_stop('tensors.diminfo','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine diminfo(Ts)

	class(tensors),intent(in)::Ts

	select case(Ts%type)
	case('tensor')
		call Ts%T%diminfo()
	case('symtensor')
		call Ts%symT%diminfo()
! 		call Ts%symT%emptyZeroBlock()
! 		call Ts%symT%write(6)
		write(*,*)'check'
 		call Ts%symT%SymCheck()
 		write(*,*)'checked'
 		write(*,*) Ts%symT%gettype()
	case('fermitensor')
		call Ts%fT%diminfo()
	case default
		call wc_error_stop('tensors.diminfo','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

function directProduct(Ts1,Ts2) result(Ts3)  ! T1 ind: 1 2, T2 ind: 3 4, result ind: 1 2 3 4

	type(tensors),intent(in)::Ts1,Ts2
	type(tensors)::Ts3

	if(Ts1%type/=Ts2%type)then
		call wc_error_stop('tensors.kron','Unmatched types ')
	end if
	call Ts3%set_type(Ts1%type)
	select case(Ts1%type)
	case('tensor')
		Ts3%T=Ts1%T.kron.Ts2%T
	case('symtensor')
		Ts3%symT=Ts1%symT.kron.Ts2%symT
	case('fermitensor')
		Ts3%fT=Ts1%fT.kron.Ts2%fT
	case default
		call wc_error_stop('tensors.kron','Unsuportted type: '//trim(Ts1%type))
	end select

end function

function directProduct2(Ts1,Ts2) result(Ts3)  ! T1 ind: 1 2, T2 ind: 3 4, result ind: 1 3 2 4

	type(tensors),intent(in)::Ts1,Ts2
	type(tensors)::Ts3

	if(Ts1%type/=Ts2%type)then
		call wc_error_stop('tensors.xx','Unmatched types ')
	end if
	call Ts3%set_type(Ts1%type)
	select case(Ts1%type)
	case('tensor')
		Ts3%T=Ts1%T.xx.Ts2%T
	case('symtensor')
		Ts3%symT=Ts1%symT.kron.Ts2%symT
		call Ts3%symT%permute([1,3,2,4])
	case('fermitensor')
		Ts3%fT=Ts1%fT.kron.Ts2%fT
		call Ts3%fT%permute([1,3,2,4])
	case default
		call wc_error_stop('tensors.xx','Unsuportted type: '//trim(Ts1%type))
	end select

end function

function addition(Ts1,Ts2) result(Ts3) 

	type(tensors),intent(in)::Ts1,Ts2
	type(tensors)::Ts3

	if(Ts1%type/=Ts2%type)then
		call wc_error_stop('tensors.kron','Unmatched types ')
	end if
	call Ts3%set_type(Ts1%type)
	select case(Ts1%type)
	case('tensor')
		Ts3%T=Ts1%T+Ts2%T
	case('symtensor')
		Ts3%symT=Ts1%symT+Ts2%symT
	case('fermitensor')
		Ts3%fT=Ts1%fT+Ts2%fT
	case default
		call wc_error_stop('tensors.kron','Unsuportted type: '//trim(Ts1%type))
	end select

end function

function minus(Ts1,Ts2) result(Ts3) 

	type(tensors),intent(in)::Ts1,Ts2
	type(tensors)::Ts3

	if(Ts1%type/=Ts2%type)then
		call wc_error_stop('tensors.kron','Unmatched types ')
	end if
	call Ts3%set_type(Ts1%type)
	select case(Ts1%type)
	case('tensor')
		Ts3%T=Ts1%T-Ts2%T
	case('symtensor')
		Ts3%symT=Ts1%symT-Ts2%symT
	case('fermitensor')
		Ts3%fT=Ts1%fT-Ts2%fT
	case default
		call wc_error_stop('tensors.kron','Unsuportted type: '//trim(Ts1%type))
	end select

end function

function conjugate(Ts1) result(Ts2)

	type(tensors),intent(in)::Ts1
	type(tensors)::Ts2

	call Ts2%set_type(Ts1%type)
	select case(Ts1%type)
	case('tensor')
		Ts2%T=.con.Ts1%T
	case('symtensor')
		Ts2%symT=.H.Ts1%symT
		write(*,*)'con1',Ts1%symT%gettype()
		write(*,*)'con2',Ts2%symT%gettype()
	case('fermitensor')
		Ts2%fT=.H.Ts1%fT
	case default
		call wc_error_stop('tensors.kron','Unsuportted type: '//trim(Ts1%type))
	end select

end function

function transpose_Ts(Ts1) result(Ts2)

	type(tensors),intent(in)::Ts1
	type(tensors)::Ts2
	character(len=max_char_length)::ind1,ind2

	call Ts2%set_type(Ts1%type)
	select case(Ts1%type)
	case('tensor')
		Ts2%T=Ts1%T.pb.1
		ind1=Ts2%T%getname(1)
		ind2=Ts2%T%getname(2)
		call Ts2%T%setname(1,ind2)
		call Ts2%T%setname(2,ind1)
	case('symtensor')
		Ts2%symT=Ts1%symT.pb.1
		ind1=Ts2%symT%getname(1)
		ind2=Ts2%symT%getname(2)
		call Ts2%symT%setname(1,ind2)
		call Ts2%symT%setname(2,ind1)
	case('fermitensor')
		Ts2%fT=Ts1%fT.pb.1
		ind1=Ts2%fT%getname(1)
		ind2=Ts2%fT%getname(2)
		call Ts2%fT%setname(1,ind2)
		call Ts2%fT%setname(2,ind1)
	case default
		call wc_error_stop('tensors.kron','Unsuportted type: '//trim(Ts1%type))
	end select

end function

function contract_routine_nochar(Ts1,Ts2) result(Ts3)

	class(tensors),intent(in)::Ts1,Ts2
	type(tensors)::Ts3

	if(Ts1%type/=Ts2%type)then
		call wc_error_stop('tensors.contract','Unmatched types ')
	end if
	call Ts3%set_type(Ts1%type)
	select case(Ts1%type)
	case('tensor')
		Ts3%T=contract(Ts1%T,Ts2%T)
	case('symtensor')
		Ts3%symT=contract(Ts1%symT,Ts2%symT)
	case default
		call wc_error_stop('tensors.contract','Unsuportted type: '//trim(Ts1%type))
	end select

end function

function contract_routine_char(Ts1,ind1,Ts2,ind2) result(Ts3)

	class(tensors),intent(in)::Ts1,Ts2
	character(len=*),intent(in)::ind1,ind2
	type(tensors)::Ts3

	if(Ts1%type/=Ts2%type)then
		call wc_error_stop('tensors.contract','Unmatched types ')
	end if
	call Ts3%set_type(Ts1%type)
	select case(Ts1%type)
	case('tensor')
		Ts3%T=contract(Ts1%T,ind1,Ts2%T,ind2)
	case('symtensor')
		Ts3%symT=contract(Ts1%symT,ind1,Ts2%symT,ind2)
	case('fermitensor')
		Ts3%fT=contract(Ts1%fT,ind1,Ts2%fT,ind2)
	case default
		call wc_error_stop('tensors.contract','Unsuportted type: '//trim(Ts1%type))
	end select

end function

function contract_routine_chars(Ts1,ind1,Ts2,ind2) result(Ts3)

	class(tensors),intent(in)::Ts1,Ts2
	character(len=*),intent(in)::ind1(:),ind2(:)
	type(tensors)::Ts3

	if(Ts1%type/=Ts2%type)then
		call wc_error_stop('tensors.contract','Unmatched types ')
	end if
	call Ts3%set_type(Ts1%type)
	select case(Ts1%type)
	case('tensor')
		Ts3%T=contract(Ts1%T,ind1,Ts2%T,ind2)
	case('symtensor')
		Ts3%symT=contract(Ts1%symT,ind1,Ts2%symT,ind2)
	case('fermitensor')
		Ts3%fT=contract(Ts1%fT,ind1,Ts2%fT,ind2)
	case default
		call wc_error_stop('tensors.contract','Unsuportted type: '//trim(Ts1%type))
	end select

end function

subroutine contract_char(Ts1,ind1,Ts2,ind2)

	class(tensors),intent(inout)::Ts1
	class(tensors),intent(in)::Ts2
	character(len=*),intent(in)::ind1,ind2

	if(Ts1%type/=Ts2%type)then
		call wc_error_stop('tensors.contract','Unmatched types ')
	end if
	select case(Ts1%type)
	case('tensor')
		call Ts1%T%contract(ind1,Ts2%T,ind2)
	case('symtensor')
		call Ts1%symT%contract(ind1,Ts2%symT,ind2)
	case('fermitensor')
		call Ts1%fT%contract(ind1,Ts2%fT,ind2)
	case default
		call wc_error_stop('tensors.contract','Unsuportted type: '//trim(Ts1%type))
	end select

end subroutine

subroutine contract_chars(Ts1,ind1,Ts2,ind2)

	class(tensors),intent(inout)::Ts1
	class(tensors),intent(in)::Ts2
	character(len=*),intent(in)::ind1(:),ind2(:)

	if(Ts1%type/=Ts2%type)then
		call wc_error_stop('tensors.contract','Unmatched types ')
	end if
	select case(Ts1%type)
	case('tensor')
		call Ts1%T%contract(ind1,Ts2%T,ind2)
	case('symtensor')
		call Ts1%symT%contract(ind1,Ts2%symT,ind2)
	case('fermitensor')
		call Ts1%fT%contract(ind1,Ts2%fT,ind2)
	case default
		call wc_error_stop('tensors.contract','Unsuportted type: '//trim(Ts1%type))
	end select

end subroutine

subroutine contract2_chars(Ts,Ts1,ind1,Ts2,ind2)

	class(tensors),intent(inout)::Ts
	class(tensors),intent(in)::Ts1,Ts2
	character(len=*),intent(in)::ind1(:),ind2(:)

	if(Ts1%type/=Ts2%type)then
		call wc_error_stop('tensors.contract','Unmatched types ')
	end if
	select case(Ts1%type)
	case('tensor')
		Ts%type='tensor'
		call Ts%T%contract(Ts1%T,ind1,Ts2%T,ind2)
	case('symtensor')
		Ts%type='symtensor'
		call Ts%symT%contract(Ts1%symT,ind1,Ts2%symT,ind2)
	case('fermitensor')
		Ts%type='fermitensor'
		call Ts%fT%contract(Ts1%fT,ind1,Ts2%fT,ind2)
	case default
		call wc_error_stop('tensors.contract','Unsuportted type: '//trim(Ts1%type))
	end select

end subroutine

function inverse(Ts) result(ivTs)

	class(tensors),intent(inout)::Ts
	type(tensors)::ivTs
	integer::i

	ivTs=Ts
	select case(Ts%type)
	case('tensor')
		ivTs%T=inverseTen(ivTs%T)
	case('symtensor')
		do i=1,ivTs%symT%dim(1)
			call ivTs%symT%setvalue([i,i],inverseTen(ivTs%symT%i([i,i])))
		end do
	case('fermitensor')
		do i=1,ivTs%fT%dim(1)
			call ivTs%fT%setvalue([i,i],inverseTen(ivTs%fT%i([i,i])))
		end do
	case default
		call wc_error_stop('tensors.inverse','Unsuportted type: '//trim(Ts%type))
	end select

end function

function get_diag(Ts) result(diag)

	class(tensors),intent(inout)::Ts
	real(8),allocatable::diag(:)
	type(tensor)::T
	integer::i,j,n

	allocate(diag(Ts%dim(1)))
	select case(Ts%type)
	case('tensor')
		do i=1,Ts%T%dim(1)
			diag(i)=Ts%T%di([i,i])
		end do
	case('symtensor')
		n=0
		do i=1,Ts%symT%dim(1)
			T=Ts%symT%i([i,i])
			do j=1,T%dim(1)
				n=n+1
				diag(n)=T%di([j,j])
			end do
		end do
	case('fermitensor')
		n=0
		do i=1,Ts%fT%dim(1)
			T=Ts%fT%i([i,i])
			do j=1,T%dim(1)
				n=n+1
				diag(n)=T%di([j,j])
			end do
		end do
	case default
		call wc_error_stop('tensors.inverse','Unsuportted type: '//trim(Ts%type))
	end select

end function

function dimTs(Ts,n) result(d)

	class(tensors),intent(inout)::Ts
	integer,intent(in)::n
	type(tensor)::T
	integer::i,d,k
	integer,allocatable::loc(:)

	select case(Ts%type)
	case('tensor')
		d=Ts%T%dim(n)
	case('symtensor')
		allocate(loc(Ts%symT%getrank()))
		loc=1
		d=0
		do i=1,Ts%symT%dim(n)
			loc(n)=i
			T=Ts%symT%i(loc)
			d=d+T%dim(n)
		end do
	case('fermitensor')
		allocate(loc(Ts%fT%getrank()))
		loc=1
		d=0
		do i=1,Ts%fT%dim(n)
			loc(n)=i
			T=Ts%fT%i(loc)
			d=d+T%dim(n)
		end do
	case default
		call wc_error_stop('tensors.inverse','Unsuportted type: '//trim(Ts%type))
	end select

end function

function inverseTen(T) result(ivT)

	type(tensor),intent(in)::T
	type(tensor)::ivT
	integer::i

	ivT=T
	do i=1,T%dim(1)
		call ivT%setvalue([i,i],1/ivT%di([i,i]))
	end do

end function

function sqrtTs(Ts) result(sqTs)

	class(tensors),intent(inout)::Ts
	type(tensors)::sqTs
	integer::i

	sqTs=Ts
	select case(Ts%type)
	case('tensor')
		sqTs%T=sqrtTen(sqTs%T)
	case('symtensor')
		do i=1,sqTs%symT%dim(1)
			call sqTs%symT%setvalue([i,i],sqrtTen(sqTs%symT%i([i,i])))
		end do
	case('fermitensor')
		do i=1,sqTs%fT%dim(1)
			call sqTs%fT%setvalue([i,i],sqrtTen(sqTs%fT%i([i,i])))
		end do
	case default
		call wc_error_stop('tensors.inverse','Unsuportted type: '//trim(Ts%type))
	end select

end function

function sqrtTen(T) result(sqT)

	type(tensor),intent(in)::T
	type(tensor)::sqT
	integer::i

	sqT=T
	do i=1,T%dim(1)
		call sqT%setvalue([i,i],sqrt(sqT%di([i,i])))
	end do

end function

type(tensors) function scale_ts(Ts,val)

	type(tensors),intent(in)::Ts
	class(*),intent(in)::val

	scale_ts%type=Ts%type

	select type(val)
	type is(real(4))
		select case(Ts%type)
		case('tensor')
			scale_ts%T=Ts%T*val
	 	case('symtensor')
	 		scale_ts%symT=Ts%symT*val
	 	case('fermitensor')
	 		scale_ts%fT=Ts%fT*val
		case default
			call wc_error_stop('tensors.scale','Unsuportted type: '//trim(Ts%type))
		end select
	type is(real(8))
		select case(Ts%type)
		case('tensor')
			scale_ts%T=Ts%T*val
	 	case('symtensor')
	 		scale_ts%symT=Ts%symT*val
	 	case('fermitensor')
	 		scale_ts%fT=Ts%fT*val
		case default
			call wc_error_stop('tensors.scale','Unsuportted type: '//trim(Ts%type))
		end select
	type is(complex(4))
		select case(Ts%type)
		case('tensor')
			scale_ts%T=Ts%T*val
	 	case('symtensor')
	 		scale_ts%symT=Ts%symT*val
	! 	case('fermitensor')
	! 		scale_ts%fT=Ts%fT*val
		case default
			call wc_error_stop('tensors.scale','Unsuportted type: '//trim(Ts%type))
		end select
	type is(complex(8))
		select case(Ts%type)
		case('tensor')
			scale_ts%T=Ts%T*val
	 	case('symtensor')
	 		scale_ts%symT=Ts%symT*val
	! 	case('fermitensor')
	! 		scale_ts%fT=Ts%fT*val
		case default
			call wc_error_stop('tensors.divide','Unsuportted type: '//trim(Ts%type))
		end select
	end select

end function

type(tensors) function divide_ts(Ts,val)

	type(tensors),intent(in)::Ts
	class(*),intent(in)::val

	divide_ts%type=Ts%type

	select type(val)
	type is(real(4))
		select case(Ts%type)
		case('tensor')
			divide_ts%T=Ts%T/val
	 	case('symtensor')
	 		divide_ts%symT=Ts%symT/val
	 	case('fermitensor')
	 		divide_ts%fT=Ts%fT/val
		case default
			call wc_error_stop('tensors.divide','Unsuportted type: '//trim(Ts%type))
		end select
	type is(real(8))
		select case(Ts%type)
		case('tensor')
			divide_ts%T=Ts%T/val
	 	case('symtensor')
	 		divide_ts%symT=Ts%symT/val
	 	case('fermitensor')
	 		divide_ts%fT=Ts%fT/val
		case default
			call wc_error_stop('tensors.divide','Unsuportted type: '//trim(Ts%type))
		end select
	type is(complex(4))
		select case(Ts%type)
		case('tensor')
			divide_ts%T=Ts%T/val
	! 	case('symtensor')
	! 		divide_ts%symT=Ts%symT/val
	! 	case('fermitensor')
	! 		divide_ts%fT=Ts%fT/val
		case default
			call wc_error_stop('tensors.divide','Unsuportted type: '//trim(Ts%type))
		end select
	type is(complex(8))
		select case(Ts%type)
		case('tensor')
			divide_ts%T=Ts%T/val
	! 	case('symtensor')
	! 		divide_ts%symT=Ts%symT/val
	! 	case('fermitensor')
	! 		divide_ts%fT=Ts%fT/val
		case default
			call wc_error_stop('tensors.divide','Unsuportted type: '//trim(Ts%type))
		end select
	end select

end function

subroutine assignmentanyTs(T1,T2)

	type(tensors),intent(in)::T2
	class(*),intent(inout)::T1

	select type(T1)
	type is(real(4))
		select case(T2%type)
		case('tensor')
			T1=T2%T
	 	case('symtensor')
	 		T1=T2%symT
	 	case('fermitensor')
	 		T1=T2%fT
		case default
			call wc_error_stop('tensors.assignment','Unsuportted type: '//trim(T2%type))
		end select
	type is(real(8))
		select case(T2%type)
		case('tensor')
			T1=T2%T
	 	case('symtensor')
	 		T1=T2%symT
	 	case('fermitensor')
	 		T1=T2%fT
		case default
			call wc_error_stop('tensors.assignment','Unsuportted type: '//trim(T2%type))
		end select
	type is(complex(4))
		select case(T2%type)
		case('tensor')
			T1=T2%T
	 	case('symtensor')
	 		T1=T2%symT
	 	case('fermitensor')
	 		T1=T2%fT
		case default
			call wc_error_stop('tensors.assignment','Unsuportted type: '//trim(T2%type))
		end select
	type is(complex(8))
		select case(T2%type)
		case('tensor')
			T1=T2%T
	 	case('symtensor')
	 		T1=T2%symT
	 	case('fermitensor')
	 		T1=T2%fT
		case default
			call wc_error_stop('tensors.assignment','Unsuportted type: '//trim(T2%type))
		end select
	type is(tensors)
		T1%type=T2%type 
		select case(T2%type)
		case('tensor')
			T1%T=T2%T
	 	case('symtensor')
	 		T1%symT=T2%symT
	 	case('fermitensor')
	 		T1%fT=T2%fT
		case default
			call wc_error_stop('tensors.assignment','Unsuportted type: '//trim(T2%type))
		end select
	end select

end subroutine

subroutine assignmentTsT(T1,T2)

	type(tensors),intent(inout)::T1
	type(tensor),intent(in)::T2

	T1%type='tensor'
	T1%T=T2

end subroutine

subroutine assignmentTssymT(T1,T2)

	type(tensors),intent(inout)::T1
	type(symtensor),intent(in)::T2

	T1%type='symtensor'
	T1%symT=T2

end subroutine

subroutine assignmentTsfT(T1,T2)

	type(tensors),intent(inout)::T1
	type(ftensor),intent(in)::T2

	T1%type='ftensor'
	T1%fT=T2

end subroutine

subroutine permute(Ts,newOrderchar)

	class(Tensors),intent(inout) :: Ts
	CHARACTER(len=*),intent(in)::newOrderchar(:)

	select case(Ts%type)
	case('tensor')
		call Ts%T%permute(newOrderchar)
	case('symtensor')
		call Ts%symT%permute(newOrderchar)
	case('fermitensor')
		call Ts%fT%permute(newOrderchar)
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine fuse(Ts,ind,len)

	class(Tensors),intent(inout) :: Ts
	integer,intent(in)::ind,len

	select case(Ts%type)
	case('tensor')
		call Ts%T%fuse(ind,len)
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine split(Ts)

	class(Tensors),intent(inout) :: Ts

	select case(Ts%type)
	case('tensor')
		call Ts%T%split()
	case('symtensor')
		call Ts%symT%split()
	case('fermitensor')
		call Ts%fT%split()
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

function getrank(Ts) result(r)

	class(Tensors),intent(inout) :: Ts
	integer::r

	select case(Ts%type)
	case('tensor')
		r=Ts%T%getrank()
	case('symtensor')
		r=Ts%symT%getrank()
	case('fermitensor')
		r=Ts%fT%getrank()
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end function

function expm_ts(Ts,name1,name2) result(r)
use SymDimension_typede

	type(Tensors),intent(in) :: Ts
	character(len=*)::name1(:),name2(:)
	type(Tensors)::r
	type(Tensor)::order(2),orderinfo(2)
	type(SymDimension)::dimen(2)

	r%type=Ts%type
	select case(Ts%type)
	case('tensor')		
		r%T=Ts%T.pb.name2
		call r%T%fuse(1,size(name1))
		call r%T%fuse(2,1+size(name2))
		r%T=expm(Ts%T)
		call r%T%split()
	case('symtensor')
		r%symT=Ts%symT.pb.name2
		r%symT=r%symT%SymFuse(size(name1),dimen(1),order(1),orderinfo(1),.true.,1)
		r%symT=r%symT%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,-1)
		r%symT=expm(r%symT)
		r%symT=r%symT%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		r%symT=r%symT%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	case('fermitensor')
		r%fT=Ts%fT.pb.name2
		r%fT=r%fT%SymFuse(size(name1),dimen(1),order(1),orderinfo(1),.true.,1)
		r%fT=r%fT%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,-1)
		r%fT=expm(r%fT)
		r%fT=r%fT%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		r%fT=r%fT%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end function

subroutine settype(Ts,type)

	class(Tensors),intent(inout) :: Ts
	character(len=*),intent(in) ::type

	select case(Ts%type)
	case('tensor')
		call Ts%T%settype(type)
	case('symtensor')
		call Ts%symT%settype(type)
	case('fermitensor')
		call Ts%fT%settype(type)
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine static(Ts)

	class(Tensors),intent(inout) :: Ts

	select case(Ts%type)
	case('tensor')
		call Ts%T%static()
	case('symtensor')
		call Ts%symT%static()
	case('fermitensor')
		call Ts%fT%static()
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

function dnorm(Ts) result(norm)

	class(Tensors),intent(inout) :: Ts
	real(8)::norm

	select case(Ts%type)
	case('tensor')
		norm=Ts%T%dnorm()
	case('symtensor')
		norm=Ts%symT%dnorm()
	case('fermitensor')
		norm=Ts%fT%dnorm()
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end function

function dmaxTs(Ts) result(max)

	class(Tensors),intent(inout) :: Ts
	real(8)::max

	select case(Ts%type)
	case('tensor')
		max=Ts%T%dmax()
	case('symtensor')
		max=Ts%symT%dmax()
	case('fermitensor')
		max=Ts%fT%dmax()
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end function

function getname_all(Ts) result(name)

	class(Tensors),intent(in) :: Ts
	type(tensor)::name

	select case(Ts%type)
	case('tensor')
		name=Ts%T%getname()
	case('symtensor')
		name=Ts%symT%getname()
	case('fermitensor')
		name=Ts%fT%getname()
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end function

function getname_i(Ts,i) result(name)

	class(Tensors),intent(in) :: Ts
	integer,intent(in)::i
	character(:),allocatable::name

	select case(Ts%type)
	case('tensor')
		name=Ts%T%getname(i)
	case('symtensor')
		name=Ts%symT%getname(i)
	case('fermitensor')
		name=Ts%fT%getname(i)
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end function

function outname(Ts,i) result(name)

	class(Tensors),intent(in) :: Ts
	integer,intent(in)::i
	character(:),allocatable::name

	select case(Ts%type)
	case('tensor')
		name=Ts%T%outname(i)
	case('symtensor')
		name=Ts%symT%outname(i)
	case('fermitensor')
		name=Ts%fT%outname(i)
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end function

function outtensorname(Ts,i) result(name)

	class(Tensors),intent(in) :: Ts
	integer,intent(in)::i
	character(:),allocatable::name

	select case(Ts%type)
	case('tensor')
		name=Ts%T%outtensorname(i)
	case('symtensor')
		name=Ts%symT%outtensorname(i)
	case('fermitensor')
		name=Ts%fT%outtensorname(i)
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end function

function dbl(Ts) result(val)

	class(Tensors),intent(in) :: Ts
	real(8)::val

	select case(Ts%type)
	case('tensor')
		val=Ts%T
	case('symtensor')
		val=Ts%symT
	case('fermitensor')
		val=Ts%fT
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end function

subroutine backward(Ts,inde)

	class(Tensors),intent(inout) :: Ts
	character(len=*),intent(in)::inde

	select case(Ts%type)
	case('tensor')
		call Ts%T%backward(inde)
	case('symtensor')
		call Ts%symT%backward(inde)
	case('fermitensor')
		call Ts%fT%backward(inde)
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

function subtensor(Ts,inde) result(Ts2)

	class(Tensors),intent(inout) :: Ts
	integer,intent(in)::inde(:)
	type(tensors)::Ts2

	Ts2%type=Ts%type
	select case(Ts%type)
	case('tensor')
		Ts2%T=Ts%T%subtensor(inde)
	case('symtensor')
		Ts2%symT=Ts%symT%subsymtensor(inde)
	case('fermitensor')
		Ts2%fT=Ts%fT%subsymtensor(inde)
	case default
		call wc_error_stop('tensors.allocate','Unsuportted type: '//trim(Ts%type))
	end select

end function

subroutine LQ(Ts,LQTs,Leftname,Rightname)

	class(tensors),intent(inout)::Ts
	type(tensors),intent(inout)::LQTs(2)
	character(len=*),intent(in)::Leftname,Rightname
	type(tensor)::LQT(2)

	select case(Ts%type)
	case('tensor')
		LQT=Ts%T%LQTensor(Leftname,Rightname)
		LQTs(1)=LQT(1)
		LQTs(2)=LQT(2)
	case('symtensor')
		call Ts%symT%LQ(LQTs(1)%symT,LQTs(2)%symT,Leftname,Rightname)
		LQTs(1)%type='symtensor'
		LQTs(2)%type='symtensor'
	case('fermitensor')
		call Ts%fT%LQ(LQTs(1)%fT,LQTs(2)%fT,Leftname,Rightname)
		LQTs(1)%type='fermitensor'
		LQTs(2)%type='fermitensor'
	case default
		call wc_error_stop('tensors.setvalue','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine QR(Ts,QRTs,Leftname,Rightname)

	class(tensors),intent(inout)::Ts
	type(tensors),intent(inout)::QRTs(2)
	character(len=*),intent(in)::Leftname,Rightname
	type(tensor)::QRT(2)

	select case(Ts%type)
	case('tensor')
		QRT=Ts%T%QRTensor(Leftname,Rightname)
		QRTs(1)=QRT(1)
		QRTs(2)=QRT(2)
	case('symtensor')
		call Ts%symT%QR(QRTs(1)%symT,QRTs(2)%symT,Leftname,Rightname)
		QRTs(1)%type='symtensor'
		QRTs(2)%type='symtensor'
	case('fermitensor')
		call Ts%fT%QR(QRTs(1)%fT,QRTs(2)%fT,Leftname,Rightname)
		QRTs(1)%type='fermitensor'
		QRTs(2)%type='fermitensor'
	case default
		call wc_error_stop('tensors.setvalue','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine SVD(Ts,SVDTs,Leftname,Rightname,Dcut)

	class(tensors),intent(inout)::Ts
	type(tensors),intent(inout)::SVDTs(3)
	character(len=*),intent(in)::Leftname,Rightname
	integer,optional,intent(in)::Dcut

	
	select case(Ts%type)
	case('tensor')
		call set_SVD_S_matrix_flag() 
		call Ts%T%SVD(SVDTs(1)%T,SVDTs(2)%T,SVDTs(3)%T,Leftname,Rightname,Dcut)
		SVDTs(1)%type='tensor'
		SVDTs(2)%type='tensor'
		SVDTs(3)%type='tensor'
	case('symtensor')
		call unset_SVD_S_matrix_flag() 
		call Ts%symT%SVD(SVDTs(1)%symT,SVDTs(2)%symT,SVDTs(3)%symT,Leftname,Rightname,Dcut)
		SVDTs(1)%type='symtensor'
		SVDTs(2)%type='symtensor'
		SVDTs(3)%type='symtensor'
	case('fermitensor')
		call unset_SVD_S_matrix_flag() 
		call Ts%fT%SVD(SVDTs(1)%fT,SVDTs(2)%fT,SVDTs(3)%fT,Leftname,Rightname,Dcut)
		SVDTs(1)%type='fermitensor'
		SVDTs(2)%type='fermitensor'
		SVDTs(3)%type='fermitensor'
	case default
		call wc_error_stop('tensors.setvalue','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine

subroutine MPI_BCAST_Tensors(Ts,id,ierr,MPI_COMM)
use mpi

	type(tensors),intent(inout)::Ts
	integer,intent(in)::id,MPI_COMM
	integer,intent(inout)::ierr
	integer::no

	select case(Ts%type)
	case('tensor')
		no=1
	case('symtensor')
		no=2
	case('fermitensor')
		no=3
	case default
		call wc_error_stop('tensors.setvalue','Unsuportted type: '//trim(Ts%type))
	end select

	call MPI_BCAST(no,1,MPI_integer,id,MPI_COMM,ierr)

	select case(no)
	case(1)
		Ts%type='tensor'
		call MPI_BCAST_Tensor(Ts%T,id,ierr,MPI_COMM)
	case(2)
		Ts%type='symtensor'
		call MPI_BCAST_SymTensor(Ts%symT,id,ierr,MPI_COMM)
	case(3)
		Ts%type='fermitensor'
		call MPI_BCAST_fTensor(Ts%fT,id,ierr,MPI_COMM)
	case default
		call wc_error_stop('tensors.setvalue','Unsuportted type: '//trim(Ts%type))
	end select

end subroutine
end module
