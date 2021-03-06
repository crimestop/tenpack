module tnsp_ext
use tensor_type
use tools
use string
use error
use mpi
use mod_rand
implicit none

type tnary

	type(Tensor)::tn	
	type(Tensor),allocatable::tns(:,:)

end type

contains

subroutine kill_D1_ind(T)

	type(tensor),intent(inout)::T
	integer::rank,i

	rank=T%getrank()
	do i=rank,1,-1
		if (T%dim(i)==1) then
			call T%backward(i)
			T=T%subtensor([-2,1])
		end if
	end do

end subroutine

subroutine permute_as(T,P)

	type(tensor),intent(in)::P
	type(tensor),intent(inout)::T
	type(tensor)::name

	name=P%getname()
	call T%permute(name%ai())

end subroutine

subroutine print_data(T,unit,fmt)

	type(tensor),intent(inout)::T
	integer,intent(in)::unit
	logical,intent(in)::fmt
	real(8),pointer :: ddata(:)
	complex(8),pointer :: zdata(:)

	select case (T%gettype())
	case (3)
		call T%pointer(ddata)
		if(fmt)then
			write(unit,*)ddata
		else
			write(unit)ddata
		end if
	case (5)
		call T%pointer(zdata)
		if(fmt)then
			write(unit,*)zdata
		else
			write(unit)zdata
		end if
	end select

end subroutine

subroutine HOSVD(T,uni,env,cen,names,Dc)

	character(len=*),intent(in)::names(:)
	type(tensor),intent(inout)::T,cen,uni(:),env(:)
	integer,intent(in),optional::Dc
	type(tensor)::useless,invten,invenv,env_temp,test
	character(len=max_char_length),allocatable::name_bac(:)
	integer::i,j,rank,dim_env

	rank=T%getrank()
	allocate(name_bac(rank))
	name_bac=''

	do i=1,size(names)
		do j=1,rank 
			if(T%outTensorName(j)/=names(i))then
				name_bac(j)=T%outName(j)
				call T%setname(j,'temp^_^.'//str(j))
			end if
		end do
		call T%SVDroutine(uni(i),env_temp,useless,names(i),'temp^_^',Dc)
		env(i)=eye(env_temp)
		call uni(i)%setname(uni(i)%getrank(),trim(names(i))//'.hosvd')
		call env(i)%setname(1,'env.in')
		call env(i)%setname(2,'env.out')

		do j=1,rank 
			if(len_trim(name_bac(j))/=0)then
				call T%setname('temp^_^.'//str(j),name_bac(j))
				name_bac(j)=''
			end if
		end do
	end do

	cen=T
	do i=1,size(names)
		invten=.con.uni(i)
		invenv=env(i)
		dim_env=invenv.dim.1
		do j=1,dim_env
			call invenv%setvalue([j,j],1/invenv%di([j,j]))
		end do
		invten=contract(invten,trim(names(i))//'.hosvd',invenv,'env.in')
		call invten%setname('env.out','center.hosvd'//str(i))
		cen=contract(cen,invten)
		env(i)=env(i)
	end do

end subroutine

function dbl(T) result(res)

	class(tensor), intent(in) ::T 
	real(8) :: res

	res=T
	
end function

subroutine cre_ann_number(A,A_dg,N,N2,D)	! generate (bosonic) creation & annhilation & number operators
								! N2=N*(N-1), leg: 1 in 2 out, partical number range is 0~D-1

	type(Tensor),intent(out)::A,A_dg,N,N2
	integer,intent(in)::D
	integer::i

	call A%allocate([D,D],'real*8')
	call A%setvalue(0d0)
	call A_dg%allocate([D,D],'real*8')
	call A_dg%setvalue(0d0)
	call N%allocate([D,D],'real*8')
	call N%setvalue(0d0)
	call N2%allocate([D,D],'real*8')
	call N2%setvalue(0d0)

	do i=1,D-1
		call A%setvalue([i,i+1],sqrt(real(i)))
	end do
	do i=1,D
		call N%setvalue([i,i],i-1)
		call N2%setvalue([i,i],(i-1)*(i-2))
	end do
	A_dg=transpose(A)

end subroutine cre_ann_number

subroutine spin_matrix(sx,sy,sz,s)			! generate standard matrices (as in book by H. Georgi) of s_i
						  			! of a spin-s representation. s is an int or a half-int.
									! row/column = 1 ... 2s+1 means m = -s ... s
	type(Tensor),intent(inout)::sx,sy,sz
	real(8),intent(in)::s
	real(8),allocatable::sp(:,:)	!s+ = sx + isy ; s- = sx - isy; sx = (s+ + s-)/2; sy = (-s+ + s-)/2*i
	integer::i,D

	D=floor(2*s+1.01)
	call sx%allocate([D,D],'real*8')
	call sx%setvalue(0d0)
	call sy%allocate([D,D],'complex*16')
	call sy%setvalue(0d0)
	call sz%allocate([D,D],'real*8')
	call sz%setvalue(0d0)
	allocate(sp(D,D))
	sp=0
	do i=1,D-1								!m=-s+i-1
		sp(i+1,i)=sqrt(real(i*(2*s+1-i)))	!sp(m+1,m)=sqrt((s+m+1)*(s-m))
	end do
	sx=(sp+transpose(sp))/2
	sy=(-sp+transpose(sp))/2*dcmplx(0,1)
	do i=1,D
		call sz%setvalue([i,i],-s+i-1)
	end do

	deallocate(sp)

end subroutine spin_matrix

subroutine spin_matrix2(sx,sy,sz,sp,sm,s)			! generate standard matrices (as in book by H. Georgi) of s_i
						  			! of a spin-s representation. s is an int or a half-int.
									! row/column = 1 ... 2s+1 means m = -s ... s
	type(Tensor),intent(inout)::sx,sy,sz,sp,sm
	real(8),intent(in)::s
	real(8),allocatable::spm(:,:)	!s+ = sx + isy ; s- = sx - isy; sx = (s+ + s-)/2; sy = (-s+ + s-)/2*i
	integer::i,D

	D=floor(2*s+1.01)
	call sx%allocate([D,D],'complex*16')
	call sx%setvalue(0)
	call sy%allocate([D,D],'complex*16')
	call sy%setvalue(0)
	call sz%allocate([D,D],'complex*16')
	call sz%setvalue(0)
	allocate(spm(D,D))
	
	spm=0
	do i=1,D-1								!m=-s+i-1
		spm(i+1,i)=sqrt(real(i*(2*s+1-i)))	!sp(m+1,m)=sqrt((s+m+1)*(s-m))
	end do
	sx=(spm+transpose(spm))/2
	sy=(-spm+transpose(spm))/2*dcmplx(0,1)
	do i=1,D
		call sz%setvalue([i,i],-s+i-1)
	end do

	sp=spm 
	sm=transpose(spm)

	deallocate(spm)

end subroutine spin_matrix2

subroutine set_Dc(myten,Dc,randomscal)
implicit none

	type(tensor),pointer,intent(inout)::myten
	integer,intent(in)::Dc
	real(8),intent(in)::randomscal
	integer::j,ind,dim,indnum
	type(tensor)::allName

	allName=myten%outAllname('left')
	indnum=allName%getTotalData()
	if(indnum/=0)then
		if(indnum>1) call wc_error_stop('set_Dc','More than one left')
		ind=myten%nameOrder(allName%ai(1))
		dim=myten%dim(ind)
		if(dim<Dc) then
			call myten%enlarge(ind,Dc,randomscal)
		else if(dim>Dc) then
			myten=myten%subTensor(ind,[1,Dc]) 
		end if
	end if

	allName=myten%outAllname('right')
	indnum=allName%getTotalData()
	if(indnum/=0)then
		if(indnum>1) call wc_error_stop('set_Dc','More than one right')
		ind=myten%nameOrder(allName%ai(1))
		dim=myten%dim(ind)
		if(dim<Dc) then
			call myten%enlarge(ind,Dc,randomscal)
		else if(dim>Dc) then
			myten=myten%subTensor(ind,[1,Dc]) 
		end if
	end if
	
end subroutine

subroutine direct_sum(ten1,ten2,ten3,freeze)

	type(tensor),intent(inout)::ten1,ten2,ten3
	character(len=*),intent(in)::freeze(:)
	integer::i,ind,dim,rank
	type(tensor)::name
	logical,allocatable::freeze_tag(:)
	integer,allocatable::dim1(:),dim2(:),dim_shift(:),pos2(:),pos_new(:)

	name=ten1%getname()
	call ten2%permute(name%ai())

	rank=ten1%getrank()
	allocate(freeze_tag(rank))
	allocate(dim1(rank))
	dim1=ten1%dim()
	allocate(dim2(rank))
	dim2=ten2%dim()

	freeze_tag=.false.
	do i=1,size(freeze)
		ind=ten1%nameOrder(freeze(i))
		freeze_tag(ind)=.true.
	end do

	ten3=ten1
	allocate(dim_shift(rank))
	do i=1,rank
		if(freeze_tag(i))then
			dim_shift(i)=0
		else
			dim_shift(i)=dim1(i)
			call ten3%enlarge(i,dim1(i)+dim2(i),0)
		end if
	end do

	allocate(pos2(rank))
	allocate(pos_new(rank))
	do i=1,ten2%getTotalData()
		call ind2pos(i,dim2,pos2)
		pos_new=pos2+dim_shift
		call ten3%setvalue(pos_new,ten2%i(pos2))
	end do
	
end subroutine

subroutine ind2pos(ind,dims,pos)

	integer,intent(in)::ind,dims(:)
	integer,intent(inout)::pos(:)
	integer::temp,i

	temp=ind
	do i=1,size(pos)
		pos(i)=temp-dims(i)*(temp/dims(i))+1
		temp=temp/dims(i)
	end do

end subroutine

function randten_sign(ten,th1,th2,rand_grad,n1,n2,tot) result(rand_ten)

	type(tensor),intent(in)::ten
	type(tensor)::rand_ten
	real(8),intent(in)::th1,th2
	real(8),pointer::dpointer(:)
	complex(8),pointer::zpointer(:)
	type(randomer),intent(inout)::rand_grad
	integer,intent(inout)::n1,n2,tot
	real(8)::grad_elem,grad_elem_re,grad_elem_im
	integer::l

	rand_ten=ten
	tot=tot+rand_ten%getTotalData()
	select case(rand_ten%gettype())
	case(3)
		call rand_ten%pointer(dpointer)
		do l=1, rand_ten%getTotalData()
			if(abs(dpointer(l))>th1)then
				grad_elem=sign(rand_grad%randreal(),dpointer(l))
				n1=n1+1
			else if(abs(dpointer(l))>th2)then
				grad_elem=rand_grad%randreal()*dpointer(l)/th1
				n2=n2+1
			else
				grad_elem=0
			end if
			dpointer(l)=grad_elem
		enddo
	case(5)
		call rand_ten%pointer(zpointer)
		do l=1, rand_ten%getTotalData()
			if(abs(zpointer(l))>th1)then
				grad_elem_re=sign(rand_grad%randreal(),real(zpointer(l)))
				grad_elem_im=sign(rand_grad%randreal(),imag(zpointer(l)))
				n1=n1+1
			else if(abs(zpointer(l))>th2)then
				grad_elem_re=rand_grad%randreal()*dble(zpointer(l))/th1
				grad_elem_im=rand_grad%randreal()*imag(zpointer(l))/th1
				n2=n2+1
			else
				grad_elem_re=0
				grad_elem_im=0
			end if
			zpointer(l)=dcmplx(grad_elem_re,grad_elem_im)
		enddo
	end select

end function

!!! mpi

subroutine wc_allreduce_Tensor(inTensor,outTensor,OP,MPIcommon,ierr)

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
		call wc_error_stop('wc_allreduce_Tensor','There is no data in one or some Tensors')
	end if
	
	if(test_same_type(inTensor,mpi_comm)==0)then	! if the Tensor is the same data type
		call wc_error_stop('wc_allreduce_Tensor','The Data type in the Tensors are not the sames')
	end if

	if(test_same_length(inTensor,mpi_comm)==0)then	! if the length of the Tensor is the same
		call wc_error_stop('wc_allreduce_Tensor','The length of the Tensor is not the same')
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

subroutine wc_reduce_Tensor(inTensor,outTensor,OP,root,MPIcommon,ierr)

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
		call wc_error_stop('wc_reduce_Tensor','The input root is illegal')
	end if
	
	if(test_not_empty(inTensor,mpi_comm)==0)then	! if the Tensor is empty
		call wc_error_stop('wc_reduce_Tensor','There is no data in one or some Tensors')
	end if
	
	if(test_same_type(inTensor,mpi_comm)==0)then	! if the Tensor is the same data type
		call wc_error_stop('wc_reduce_Tensor','The Data type in the Tensors are not the sames')
	end if

	if(test_same_length(inTensor,mpi_comm)==0)then	! if the length of the Tensor is the same
		call wc_error_stop('wc_reduce_Tensor','The length od the Tensor is not the same')
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
