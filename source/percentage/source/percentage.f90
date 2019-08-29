module mod_percent
use mod_mpi_info
use error
implicit none
private
real(8)::perc_save= 0d0
integer::line_length=80

public percent,percent_end 

contains

subroutine percent(words_in,perc_in,step_)

	real(8),intent(in)::perc_in
	character(len=*),intent(in)::words_in
	real(8),intent(in),optional::step_
	real(8)::step
	character(len=line_length)::words

	if(present(step_))then
		step=step_
	else
		step=1d0
	end if

	if(len_trim(words_in)+1>line_length)then
		call wc_error_stop('percent','input words are too long.')
	end if
	words=''
	words(1:1)=char(13)
	words(2:)=words_in

	if(abs(perc_in-perc_save)>=step)then
		if(my_rank==0)then
			write(*,'(a,$)') words
		end if
		perc_save=perc_in
	end if

end subroutine

subroutine percent_end(words_in)

	character(len=*),intent(in)::words_in
	character(len=line_length)::words

	if(len_trim(words_in)+1>line_length)then
		call wc_error_stop('percent','input words are too long.')
	end if
	words=''
	words(1:1)=char(13)
	words(2:)=words_in
	
	if(my_rank==0) then
		write(*,'(a)')words
	end if
	perc_save=0d0

end subroutine

end module