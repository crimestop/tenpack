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
	integer::message_replacing_status=0
	integer::line_length=80

public nproc,my_rank,ierror,set_output_log_TNSG,unset_output_log_TNSG,write_message,&
	dynamic_message,dynamic_message_end
contains

subroutine minimal_error_stop(message)
	character(len=*),intent(in) :: message
	integer,pointer::t

	if(my_rank==0) then
		write(*,*)trim(message)
		if(log_unit>0)then
			write(log_unit,'(a)')trim(message)
			flush(log_unit)
		end if
		t=0
		if(nproc>1) call MPI_Finalize(ierror)
		stop
	end if

end subroutine

subroutine set_output_log_TNSG(log,file)
	!! func write_message will also print in the log file
	integer,intent(in) :: log
	!! the unit of the log file
	character(len=*),intent(in) :: file
	!! the path of the log file
	integer,pointer::t

	if(log<=0)then
		call minimal_error_stop('Error in set_output_log_TNSG, log_unit <= 0!')
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
	!! the message to write, to trim

	if(my_rank==0) then
		write(*,*)trim(message)
		if(log_unit>0)then
			write(log_unit,'(a)')trim(message)
			flush(log_unit)
		end if
	end if
    
end subroutine 

subroutine dynamic_message(message)
	!! write message in the core with rank = 0, keep replacing a line by a new message
	character(len=*),intent(in) :: message
	!! the message to write, not to trim
	character(len=line_length)::words

	if(my_rank==0) then
		words=message
		write(*,'(a,$)') char(13)//words
		if(log_unit>0)then
			if (message_replacing_status==1) backspace(log_unit)
			write(log_unit,'(a)') message
			flush(log_unit)
		end if
		if (message_replacing_status==0) message_replacing_status=1
	end if
    
end subroutine 

subroutine dynamic_message_end(message)
	!! write message in the core with rank = 0, stop replacing a line by a new message
	character(len=*),intent(in) :: message
	!! the message to write, not to trim
	character(len=line_length)::words

	if(my_rank==0) then
		words=message
		write(*,'(a)') char(13)//words
		if(log_unit>0)then
			if (message_replacing_status==1) backspace(log_unit)
			write(log_unit,'(a)') message
			flush(log_unit)
		end if
		message_replacing_status=0
	end if
    
end subroutine 

end module