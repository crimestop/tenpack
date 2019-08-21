module mod_percent
use mod_mpi_info
implicit none
private
real(8)::perc_save= 0d0

public percent,percent_end 

contains

subroutine percent(perc_in,step_,words_before_,words_after_)

	real(8),intent(in)::perc_in
	character(len=*),intent(in),optional::words_before_,words_after_
	real(8),intent(in),optional::step_
	real(8)::step
	character(:),allocatable::words_before,words_after

	if(present(step_))then
		step=step_
	else
		step=1d0
	end if
	if(present(words_before_))then
		words_before=words_before_
	else
		words_before=''
	end if
	if(present(words_after_))then
		words_after=', '//words_after_
	else
		words_after=''
	end if

	if(abs(perc_in-perc_save)>=step)then
		if(my_rank==0)then
			if(perc_in<0)then
				write(*,'(1a1,a,F4.2,a,$)')char(13),words_before//' 0% finished'//words_after//'    '
			else if(perc_in<10)then
				write(*,'(1a1,a,F4.2,a,$)')char(13),words_before//' ',perc_in,'% finished'//words_after//' '
			else if (perc_in<100)then
				write(*,'(1a1,a,F5.2,a,$)')char(13),words_before//' ',perc_in,'% finished'//words_after
			else
				write(*,'(1a1,a,$)')char(13),words_before//' 100% finished'//words_after//'  '
			end if
		end if
		perc_save=perc_in
	end if

end subroutine

subroutine percent_end(words_before_,words_after_)

	character(len=*),intent(in),optional::words_before_,words_after_
	character(:),allocatable::words_before,words_after

	if(present(words_before_))then
		words_before=words_before_
	else
		words_before=''
	end if
	if(present(words_after_))then
		words_after=', '//words_after_
	else
		words_after=''
	end if
	
	if(my_rank==0) then
		write(*,'(1a1,a,$)')char(13),'                                                                      ' ! erase last line
		write(*,'(1a1,a)')char(13),words_before//' 100% finished'//words_after
	end if
	perc_save=0d0

end subroutine

end module