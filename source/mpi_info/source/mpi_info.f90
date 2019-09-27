module mod_mpi_info
!! (in libkernel)
!! the package to deal with mpi
implicit none
private

	integer::nproc=1
	!! number of total cores
	integer::my_rank=0
	!! rank of the current core
	integer::log_unit=-1
	integer::ierror
	!! mpi error information

public nproc,my_rank,ierror,set_output_log_TNSG,unset_output_log_TNSG,write_message
contains

subroutine set_output_log_TNSG(log,file)
	!! func write_message will also print in the log file
	integer,intent(in) :: log
	!! the unit of the log file
	character(len=*),intent(in) :: file
	!! the path of the log file

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
	!! func write_message will not print in the log file

	log_unit=-1

end subroutine

subroutine write_message(message)
	!! write message in the core with rank = 0
	character(len=*),intent(in) :: message
	!! the message to write

	if(my_rank==0) then
		write(*,*)trim(message)
		if(log_unit>0) write(log_unit,*)trim(message)
	end if
    
end subroutine 

end module