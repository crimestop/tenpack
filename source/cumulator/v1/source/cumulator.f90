module mod_cumu
use tensor_type
use otherFunction
use error
implicit none
private

type cumulator
	private
	real(8)::dcumu
	complex(8)::zcumu
	type(tensor)::tcumu
	real(8)::weight
	character(len=1)::type='f' ! f means undefined

contains
	private
	procedure,public::init
	procedure::dadd
	procedure::zadd
	procedure::tadd
	generic,public::add=>dadd,zadd,tadd
	procedure,public::clean
	procedure,public::dave
	procedure,public::zave
	procedure,public::tave

end type

public cumulator

contains

subroutine init(C,type,T)

	class(cumulator),intent(inout) :: C
	character(len=1),intent(in) :: type
	class(tensor),intent(in),optional ::T
	
	select case(type)
	case('d')
		C%type='d'
	case('z')
		C%type='z'
	case('t')
		C%type='t'
		if(.not.present(T))then
			call wc_error_stop('cumu.init','tensor prototype needed!')
		else
			call C%tcumu%allocate(T)
			call C%tcumu%static()
		end if
	case default
		call wc_error_stop('cumu.init','input type unidentified!')
	end select
	call C%clean()

end subroutine

subroutine dadd(C,val,weight_)

	class(cumulator),intent(inout) :: C
	real(8),intent(in)::val
	real(8),intent(in),optional::weight_
	real(8) :: weight
	type(tensor)::name
	
	if(present(weight_))then
		weight=weight_
	else
		weight=1d0
	end if

	select case(C%type)
	case('d')
		C%dcumu=C%dcumu+val*weight
	case('z')
		C%zcumu=C%zcumu+val*weight
	case('t')
		if(.not. abs(val)<1d-10)then ! otherwise 0 is added
			call wc_error_stop('cumu.add','tensor input needed!')
		end if
	case('f')
		call wc_error_stop('cumu.add','cumulator used before initiated!')
	end select

	C%weight=C%weight+weight

end subroutine

subroutine zadd(C,val,weight_)

	class(cumulator),intent(inout) :: C
	complex(8),intent(in)::val
	real(8),intent(in),optional::weight_
	real(8) :: weight
	type(tensor)::name
	
	if(present(weight_))then
		weight=weight_
	else
		weight=1d0
	end if
	
	select case(C%type)
	case('d')
		C%dcumu=C%dcumu+val*weight
	case('z')
		C%zcumu=C%zcumu+val*weight
	case('t')
		call wc_error_stop('cumu.add','tensor input needed!')
	case('f')
		call wc_error_stop('cumu.add','cumulator used before initiated!')
	end select
	
	C%weight=C%weight+weight

end subroutine

subroutine tadd(C,val,weight_)

	class(cumulator),intent(inout) :: C
	class(tensor),intent(inout)::val
	real(8),intent(in),optional::weight_
	real(8) :: weight
	type(tensor)::name
	
	if(present(weight_))then
		weight=weight_
	else
		weight=1d0
	end if
	
	select case(C%type)
	case('d','z')
		call wc_error_stop('cumu.add','real or complex input needed!')
	case('t')
		name=C%tcumu%getname()
		call val%permute(name%ai())
		C%tcumu=C%tcumu+val*weight
	case('f')
		call wc_error_stop('cumu.add','cumulator used before initiated!')
	end select
	
	C%weight=C%weight+weight

end subroutine

subroutine clean(C)

	class(cumulator),intent(inout) :: C

	select case(C%type)
	case('d')
		C%dcumu=0d0
	case('z')
		C%zcumu=dcmplx(0d0,0d0)
	case('t')
		call C%tcumu%zero()
	case('f')
		call wc_error_stop('cumu.clean','cumulator used before initiated!')
	end select
	C%weight=0d0

end subroutine

function dave(C,comm,root) result(res)

	class(cumulator),intent(inout) :: C
	integer,intent(in),optional::comm,root
	integer::my_rank,nproc,ierr
	real(8)::res,totcumu,totweight

	select case(C%type)
	case('d')
		if(present(comm))then
			if(present(root))then
				call MPI_Comm_size(comm,nproc, ierr)
				call MPI_Comm_rank(MPI_COMM_WORLD,my_rank,ierr)
				if(root>=nproc) call wc_error_stop('cumu.dave','root id >= number of processors!')
				call MPI_reduce(C%dcumu,totcumu,1,MPI_REAL8,MPI_SUM,root,comm,ierr)
				call MPI_reduce(C%weight,totweight,1,MPI_REAL8,MPI_SUM,root,comm,ierr)
				if(my_rank==root)then
					if(totweight==0d0) call wc_error_stop('cumu.dave','total weight is 0!')
					res=totcumu/totweight
				end if
			else
				call MPI_allreduce(C%dcumu,totcumu,1,MPI_REAL8,MPI_SUM,comm,ierr)
				call MPI_allreduce(C%weight,totweight,1,MPI_REAL8,MPI_SUM,comm,ierr)
				if(totweight==0d0) call wc_error_stop('cumu.dave','total weight is 0!')
				res=totcumu/totweight
			end if 
		else
			if(C%weight==0d0) call wc_error_stop('cumu.dave','total weight is 0!')
			res=C%dcumu/C%weight
		end if
	case('z')
		call wc_error_stop('cumu.dave','please use zave for complex value!')
	case('t')
		call wc_error_stop('cumu.dave','please use tave for tensor value!')
	case('f')
		call wc_error_stop('cumu.dave','cumulator used before initiated!')
	end select

end function

function zave(C,comm,root) result(res)

	class(cumulator),intent(inout) :: C
	integer,intent(in),optional::comm,root
	integer::my_rank,nproc,ierr
	complex(8)::res,totcumu
	real(8)::totweight

	select case(C%type)
	case('d')
		call wc_error_stop('cumu.zave','please use dave for real value!')
	case('z')
		if(present(comm))then
			if(present(root))then
				call MPI_Comm_size(comm,nproc, ierr)
				call MPI_Comm_rank(comm,my_rank,ierr)
				if(root>=nproc) call wc_error_stop('cumu.zave','root id >= number of processors!')
				call MPI_reduce(C%zcumu,totcumu,1,MPI_DOUBLE_COMPLEX,MPI_SUM,root,comm,ierr)
				call MPI_reduce(C%weight,totweight,1,MPI_REAL8,MPI_SUM,root,comm,ierr)
				if(my_rank==root)then
					if(totweight==0d0) call wc_error_stop('cumu.zave','total weight is 0!')
					res=totcumu/totweight
				end if
			else
				call MPI_allreduce(C%zcumu,totcumu,1,MPI_DOUBLE_COMPLEX,MPI_SUM,comm,ierr)
				call MPI_allreduce(C%weight,totweight,1,MPI_REAL8,MPI_SUM,comm,ierr)
				if(totweight==0d0) call wc_error_stop('cumu.zave','total weight is 0!')
				res=totcumu/totweight
			end if 
		else
			if(C%weight==0d0) call wc_error_stop('cumu.zave','total weight is 0!')
			res=C%zcumu/C%weight
		end if
	case('t')
		call wc_error_stop('cumu.zave','please use tave for tensor value!')
	case('f')
		call wc_error_stop('cumu.zave','cumulator used before initiated!')
	end select

end function

function tave(C,comm,root) result(res)

	class(cumulator),intent(inout) :: C
	integer,intent(in),optional::comm,root
	integer::my_rank,nproc,ierr
	real(8)::totweight
	type(tensor)::res,totcumu

	select case(C%type)
	case('d')
		call wc_error_stop('cumu.tave','please use dave for real value!')
	case('z')
		call wc_error_stop('cumu.tave','please use zave for complex value!')
	case('t')
		if(present(comm))then
			if(present(root))then
				call MPI_Comm_size(comm,nproc, ierr)
				call MPI_Comm_rank(comm,my_rank,ierr)
				if(root>=nproc) call wc_error_stop('cumu.tave','root id >= number of processors!')
				call reduce_Tensor(C%tcumu,totcumu,MPI_SUM,root,comm,ierr)
				call MPI_reduce(C%weight,totweight,1,MPI_REAL8,MPI_SUM,root,comm,ierr)
				if(my_rank==root)then
					if(totweight==0d0) call wc_error_stop('cumu.tave','total weight is 0!')
					res=totcumu/totweight
				end if
			else
				call allreduce_Tensor(C%tcumu,totcumu,MPI_SUM,comm,ierr)
				call MPI_allreduce(C%weight,totweight,1,MPI_REAL8,MPI_SUM,comm,ierr)
				if(totweight==0d0) call wc_error_stop('cumu.tave','total weight is 0!')
				res=totcumu/totweight
			end if 
		else
			if(C%weight==0d0) call wc_error_stop('cumu.tave','total weight is 0!')
			res=C%tcumu/C%weight
		end if
	case('f')
		call wc_error_stop('cumu.tave','cumulator used before initiated!')
	end select

end function

end module 
