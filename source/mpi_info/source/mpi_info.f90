module mod_mpi_info
implicit none
private

	integer::nproc=1
	integer::my_rank=0
	integer::log_unit=-1
	integer::ierror

public nproc,my_rank,ierror,set_output_log_TNSG,unset_output_log_TNSG,write_message
contains

subroutine set_output_log_TNSG(log,file)
	integer,intent(in) :: log
	character(len=*),intent(in) :: file

	if(log<=0)then
		call write_message('Error in set_output_log_TNSG, log_unit <= 0!')
		if(nproc>1) call MPI_Finalize(ierror)
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

	if(my_rank==0) then
		write(*,*)trim(message)
		if(log_unit>0) write(log_unit,*)trim(message)
	end if
    
end subroutine 

end module