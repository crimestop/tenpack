module mod_mpi_info
implicit none

	integer::nproc=1
	integer::my_rank=0
	integer::log_unit=-1
	integer::ierr

contains

subroutine set_output_log_TNSG(log,file)
	integer,intent(in) :: log
	character(len=*),intent(in) :: file

	if(log<=0)then
		call write_message('Error in set_output_log_TNSG, log_unit <= 0!')
		if(nproc>1) call MPI_Finalize(ierr)
		stop
	else 
		log_unit=log
		open(unit=log_unit,file=file)
	end if

end subroutine

subroutine unset_output_log_TNSG()

	log_unit=-1

end subroutine

subroutine write_message(message)
	character(len=*),intent(in) :: message

	write(*,*)message
	if(my_rank==0 .and. log_unit>0) write(log_unit,*)message
    
end subroutine 

end module