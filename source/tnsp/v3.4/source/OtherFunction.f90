module OtherFunction
use Tensor_type
use tools
use mpi
implicit none

contains

subroutine ALLREDUCE_Tensor(inTensor,outTensor,OP,MPIcommon,ierr)

	type(Tensor),target,intent(in)::inTensor
	type(Tensor),target,intent(inout)::outTensor
	integer,intent(inout)::ierr
	integer,intent(in)::op
	integer,optional,intent(in)::MPIcommon
	integer::proID,proNum,length,mpi_comm
	type(Tensor),pointer::p1,p2
	integer,pointer::idata(:),idata2(:)
	real*4,pointer::sdata(:),sdata2(:)
	real*8,pointer::ddata(:),ddata2(:)
	complex*8,pointer::cdata(:),cdata2(:)
	complex*16,pointer::zdata(:),zdata2(:)
	logical,pointer::ldata(:),ldata2(:)
	character(len=characterlen),pointer::adata(:),adata2(:)
	logical::in_place

	if(present(MPIcommon))then
		mpi_comm=MPIcommon
	else
		mpi_comm=mpi_comm_world
	end if
	call mpi_comm_rank(mpi_comm,proID,ierr)
	call mpi_comm_size(mpi_comm,proNum,ierr )
	
	if(test_not_empty(inTensor,mpi_comm)==0)then	! if the Tensor is empty
		call writemess('ERROR in ALLREDUCE_Tensor,the is no date in one or some Tensors')
		call error_stop
	end if
	
	if(test_same_type(inTensor,mpi_comm)==0)then	! if the Tensor is the same data type
		call writemess('ERROR in ALLREDUCE_Tensor,the Data type in the Tensors are not the sames')
		call error_stop
	end if

	if(test_same_length(inTensor,mpi_comm)==0)then	! if the length of the Tensor is the same
		call writemess('ERROR in ALLREDUCE_Tensor,the length od the Tensor is not the same')
		call error_stop
	end if

	in_place=test_same_tensor(inTensor,outTensor)

	if(.not. in_place)then
		call outTensor%empty()
		call outTensor%allocate(inTensor)
	end if

	length=inTensor%getTotalData()
	select case(inTensor%getType())
		case(1)
			call inTensor%pointer(idata)
			call outTensor%pointer(idata2)
			if(in_place)then
				call MPI_ALLREDUCE(MPI_IN_PLACE,idata2,length,MPI_INTEGER,OP,mpi_comm,ierr)
			else
				call MPI_ALLREDUCE(idata,idata2,length,MPI_INTEGER,OP,mpi_comm,ierr)
			end if
		case(2)
			call inTensor%pointer(sdata)
			call outTensor%pointer(sdata2)
			if(in_place)then
				call MPI_ALLREDUCE(MPI_IN_PLACE,sdata2,length,MPI_real,OP,mpi_comm,ierr)
			else
				call MPI_ALLREDUCE(sdata,sdata2,length,MPI_real,OP,mpi_comm,ierr)
			end if
		case(3)
			call inTensor%pointer(ddata)
			call outTensor%pointer(ddata2)
			if(in_place)then
				call MPI_ALLREDUCE(MPI_IN_PLACE,ddata2,length,MPI_double_precision,OP,mpi_comm,ierr)
			else
				call MPI_ALLREDUCE(ddata,ddata2,length,MPI_double_precision,OP,mpi_comm,ierr)
			end if
		case(4)
			call inTensor%pointer(cdata)
			call outTensor%pointer(cdata2)
			if(in_place)then
				call MPI_ALLREDUCE(MPI_IN_PLACE,cdata2,length,MPI_complex,OP,mpi_comm,ierr)
			else
				call MPI_ALLREDUCE(cdata,cdata2,length,MPI_complex,OP,mpi_comm,ierr)
			end if
		case(5)
			call inTensor%pointer(zdata)
			call outTensor%pointer(zdata2)
			if(in_place)then
				call MPI_ALLREDUCE(MPI_IN_PLACE,zdata2,length,MPI_double_complex,OP,mpi_comm,ierr)
			else
				call MPI_ALLREDUCE(zdata,zdata2,length,MPI_double_complex,OP,mpi_comm,ierr)
			end if
		case(6)
			call inTensor%pointer(ldata)
			call outTensor%pointer(ldata2)
			if(in_place)then
				call MPI_ALLREDUCE(MPI_IN_PLACE,ldata2,length,MPI_logical,OP,mpi_comm,ierr)
			else
				call MPI_ALLREDUCE(ldata,ldata2,length,MPI_logical,OP,mpi_comm,ierr)
			end if
		case(7)
			call inTensor%pointer(adata)
			call outTensor%pointer(adata2)
			if(in_place)then
				call MPI_ALLREDUCE(MPI_IN_PLACE,adata2,length,MPI_character,OP,mpi_comm,ierr)
			else
				call MPI_ALLREDUCE(adata,adata2,length,MPI_character,OP,mpi_comm,ierr)
			end if
	end  select

end subroutine

subroutine REDUCE_Tensor(inTensor,outTensor,OP,root,MPIcommon,ierr)

	type(Tensor),target,intent(in)::inTensor
	type(Tensor),target,intent(inout)::outTensor
	integer,intent(inout)::ierr
	integer,intent(in)::op
	integer,optional,intent(in)::MPIcommon,root
	integer::proID,proNum,length,mpi_comm,mpi_root
	type(Tensor),pointer::p1,p2
	integer,pointer::idata(:),idata2(:)
	real*4,pointer::sdata(:),sdata2(:)
	real*8,pointer::ddata(:),ddata2(:)
	complex*8,pointer::cdata(:),cdata2(:)
	complex*16,pointer::zdata(:),zdata2(:)
	logical,pointer::ldata(:),ldata2(:)
	character(len=characterlen),pointer::adata(:),adata2(:)
	logical::in_place

	if(present(MPIcommon))then
		mpi_comm=MPIcommon
	else
		mpi_comm=mpi_comm_world
	end if
	if(present(root))then
		mpi_root=root
	else
		mpi_root=0
	end if
	call mpi_comm_rank(mpi_comm,proID,ierr)
	call mpi_comm_size(mpi_comm,proNum,ierr )
	if(root>=proNum .or. root<0)then
		call writemess('ERROR in REDUCE_Tensor,input root is illegal')
		call error_stop
	end if
	
	if(test_not_empty(inTensor,mpi_comm)==0)then	! if the Tensor is empty
		call writemess('ERROR in REDUCE_Tensor,the is no date in one or some Tensors')
		call error_stop
	end if
	
	if(test_same_type(inTensor,mpi_comm)==0)then	! if the Tensor is the same data type
		call writemess('ERROR in REDUCE_Tensor,the Data type in the Tensors are not the sames')
		call error_stop
	end if

	if(test_same_length(inTensor,mpi_comm)==0)then	! if the length of the Tensor is the same
		call writemess('ERROR in REDUCE_Tensor,the length od the Tensor is not the same')
		call error_stop
	end if

	in_place=test_same_tensor(inTensor,outTensor)

	if(.not. in_place)then
		call outTensor%empty()
		call outTensor%allocate(inTensor)
	end if

	length=inTensor%getTotalData()
	select case(inTensor%getType())
		case(1)
			call inTensor%pointer(idata)
			call outTensor%pointer(idata2)
			if(in_place)then
				call MPI_REDUCE(MPI_IN_PLACE,idata2,length,MPI_INTEGER,OP,mpi_root,mpi_comm,ierr)
			else
				call MPI_REDUCE(idata,idata2,length,MPI_INTEGER,OP,mpi_root,mpi_comm,ierr)
			end if
		case(2)
			call inTensor%pointer(sdata)
			call outTensor%pointer(sdata2)
			if(in_place)then
				call MPI_REDUCE(MPI_IN_PLACE,sdata2,length,MPI_real,OP,mpi_root,mpi_comm,ierr)
			else
				call MPI_REDUCE(sdata,sdata2,length,MPI_real,OP,mpi_root,mpi_comm,ierr)
			end if
		case(3)
			call inTensor%pointer(ddata)
			call outTensor%pointer(ddata2)
			if(in_place)then
				call MPI_REDUCE(MPI_IN_PLACE,ddata2,length,MPI_double_precision,OP,mpi_root,mpi_comm,ierr)
			else
				call MPI_REDUCE(ddata,ddata2,length,MPI_double_precision,OP,mpi_root,mpi_comm,ierr)
			end if
		case(4)
			call inTensor%pointer(cdata)
			call outTensor%pointer(cdata2)
			if(in_place)then
				call MPI_REDUCE(MPI_IN_PLACE,cdata2,length,MPI_complex,OP,mpi_root,mpi_comm,ierr)
			else
				call MPI_REDUCE(cdata,cdata2,length,MPI_complex,OP,mpi_root,mpi_comm,ierr)
			end if
		case(5)
			call inTensor%pointer(zdata)
			call outTensor%pointer(zdata2)
			if(in_place)then
				call MPI_REDUCE(MPI_IN_PLACE,zdata2,length,MPI_double_complex,OP,mpi_root,mpi_comm,ierr)
			else
				call MPI_REDUCE(zdata,zdata2,length,MPI_double_complex,OP,mpi_root,mpi_comm,ierr)
			end if
		case(6)
			call inTensor%pointer(ldata)
			call outTensor%pointer(ldata2)
			if(in_place)then
				call MPI_REDUCE(MPI_IN_PLACE,ldata2,length,MPI_logical,OP,mpi_root,mpi_comm,ierr)
			else
				call MPI_REDUCE(ldata,ldata2,length,MPI_logical,OP,mpi_root,mpi_comm,ierr)
			end if
		case(7)
			call inTensor%pointer(adata)
			call outTensor%pointer(adata2)
			if(in_place)then
				call MPI_REDUCE(MPI_IN_PLACE,adata2,length,MPI_character,OP,mpi_root,mpi_comm,ierr)
			else
				call MPI_REDUCE(adata,adata2,length,MPI_character,OP,mpi_root,mpi_comm,ierr)
			end if
	end  select

end subroutine


function test_same_tensor(T1,T2) result(res)

	logical :: res
	type(tensor),intent(in),target :: T1,T2
	type(tensor),pointer :: p1,p2

	p1=>T1
	p2=>T2
	res = associated(p1,p2)
	p1=>null()
	p2=>null()

end function

function test_not_empty(T,mpi_comm) result(res)

	integer :: res,goonFlag
	type (tensor),intent(in) :: T
	integer,intent(in) :: mpi_comm
	integer :: ierr

	if(T%getFlag()) goonFlag=0
	call MPI_ALLREDUCE(goonFlag,res,1,MPI_INTEGER,MPI_SUM,mpi_comm,ierr)
	if(res==0)then
		res=1
	else
		res=0
	end if

end function 

function test_same_type(T,mpi_comm) result(res)

	integer :: res,goonFlag
	type (tensor),intent(in) :: T
	integer,intent(in) :: mpi_comm
	integer :: ierr,classtype

	classtype=T%getType()
	call MPI_BCAST(classtype,1,MPI_integer,0,mpi_comm,ierr)
	if(T%getType() == classtype) goonFlag=0
	call MPI_ALLREDUCE(goonFlag,res,1,MPI_INTEGER,MPI_SUM,mpi_comm,ierr)
	if(res==0)then
		res=1
	else
		res=0
	end if

end function 

function test_same_length(T,mpi_comm) result(res)

	integer :: res,goonFlag
	type (tensor),intent(in) :: T
	integer,intent(in) :: mpi_comm
	integer :: ierr,length

	length=T%getTotalData()
	call MPI_BCAST(length,1,MPI_integer,0,mpi_comm,ierr)
	if(T%getTotalData() == length) goonFlag=0
	call MPI_ALLREDUCE(goonFlag,res,1,MPI_INTEGER,MPI_SUM,mpi_comm,ierr)
	if(res==0)then
		res=1
	else
		res=0
	end if

end function 

end module