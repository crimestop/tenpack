module error
use tools  ! writemess
use mod_mpi_info 
implicit none

logical :: test_tag=.false.

interface wc_error_stop
	module procedure wc_error_stop_mess2
	module procedure wc_error_stop_mess1
	module procedure wc_error_stop_nomess
end interface

contains

subroutine error_test_tag(tag)

	logical, intent(in) :: tag

	test_tag=tag
	
end subroutine

subroutine wc_error_stop_mess2(error_pos,error_mess)

	character(len=*),intent(in)::error_pos,error_mess
	integer::ierr

	call writemess('In '//trim(error_pos)//':')
	call writemess(error_mess)
	call wc_outpicture()
	if(.not. test_tag)then
		if(nproc>1) call MPI_Finalize(ierr)
		stop
	end if

end subroutine

subroutine wc_error_stop_mess1(error_mess)

	character(len=*),intent(in)::error_mess
	integer::ierr

	call writemess(error_mess)
	call wc_outpicture()
	if(.not. test_tag)then
		if(nproc>1) call MPI_Finalize(ierr)
		stop
	end if

end subroutine	

subroutine wc_error_stop_nomess()

	integer::ierr
	
	call wc_outpicture()
	if(.not. test_tag)then
		if(nproc>1) call MPI_Finalize(ierr)
		stop
	end if

end subroutine	

subroutine wc_outpicture()

	integer::pic

	call system_clock(pic) ! millisecond

	select case(modulo(pic,7)+1)
	case(1)
		call writemess('                                                 ')
		call writemess('      __                                         ')
		call writemess('     #####                    ###                ')
		call writemess('    #########___---------____#####               ')
		call writemess('    ########                 #####               ')
		call writemess('     ######                   ###                ')
		call writemess('      ####                     ##                ')
		call writemess('       ##                       \     ________________________________   ')
		call writemess('       /        ##       ##      |   |you have bugs!!                 |  ')
		call writemess('       |       #0#       #0#      |  |Report ONLY bugs of the package |  ')
		call writemess('       |      ###         ###     |  /to wang1329@mail.ustc.edu.cn !  |  ')
		call writemess('       |      #            ##     | /_________________________________|  ')
		call writemess('       |             ####         |              ')
		call writemess('      /\         \    ##   /     /               ')
		call writemess('     /  \         \_______/     /                ')
		call writemess('    ###  \              \\     ##                ')
		call writemess('   ######################\\########              ')
		call writemess(' #########################\\########             ')
		call writemess('                           \\                    ')
		call writemess('                                                 ')
	case(2)
		call writemess('                                                ')
		call writemess('                      ________                  ')
		call writemess('                    _/__|__|__\_                ')
		call writemess('                   /     _      \               ')
		call writemess('                  /_   _(_)_   __\              ')
		call writemess('                 ||_| |____o| |_|_|             ')
		call writemess('             ____|================|___          ')
		call writemess('            |    |   __________   |   \         ')
		call writemess('            |    |  | WARNING! |  |    \        ')
		call writemess('            |   ||  | you      |  |\    \       ')
		call writemess('            |   ||  |  have    |  | \    \      ')
		call writemess('            |   ||  |    bugs  |  |  \    \     ')
		call writemess('            |   ||  |__________|  |   \    \    ')
		call writemess('            |   ||                |    \____\   ')
		call writemess('            |   ||________________|      | \    ')
		call writemess('            |___|      \     /           |  \   ')
		call writemess('             / \        \___/            |___\  ')
		call writemess('            /   \       /   \                   ')
		call writemess('           /_____\     /_____\                  ')
		call writemess('_____________________________________________________________ ')
		call writemess('|Report ONLY bugs of the package to wang1329@mail.ustc.edu.cn|')
		call writemess('`~----------------------------------------------------------~`')
	case(3)
		call writemess('                                  ')
		call writemess('                    |_|           ')
		call writemess('                  _P   P_         ')
		call writemess('           \___  /  \|/  \  ___/  ')
		call writemess('               \/    |    \/      ')
		call writemess('                |  0 |   0|       ')
		call writemess('             __/| 000|  00|\__    ')
		call writemess('            /   \  00|    /   \   ')
		call writemess('              __/\___|___/\__     ')
		call writemess('             /               \    ')
		call writemess(' _______________                  ')
		call writemess('|you have bugs! |____________________________________________ ')
		call writemess('|Report ONLY bugs of the package to wang1329@mail.ustc.edu.cn|')
		call writemess('`~----------------------------------------------------------~`')
	case(4)
		call writemess('                                          ')
		call writemess('                ______                    ')
		call writemess('               /__    \                   ')
		call writemess('              /\_/     \                  ')
		call writemess('            _/__       |                  ')
		call writemess('           / \_/       |                  ')
		call writemess('      _____|_____      |               ________________________________ ')
		call writemess('     |   |   |   |     |              |you have bugs!!                 |')
		call writemess('     |   |   |   |     |              |Report ONLY bugs of the package |')
		call writemess('     |   |   |   |     /              |to wang1329@mail.ustc.edu.cn !  |')
		call writemess('     |___|___|___|_   /              /`-------------------------------- ')
		call writemess('    /  _  _   _    \_/             ()     ')
		call writemess('    | | || |.| | | |            --/|_/    ')
		call writemess('    | |_||_|.|_| | |            --\/      ')
		call writemess('    \______________/            --/-\     ')
		call writemess('     |   |   |   |               /  /     ')
		call writemess('     |   |   |   |                        ')
		call writemess('     |   |   |   |                        ')
		call writemess('     |   |   |   |                        ')
		call writemess('     |   |   |   |                        ')
		call writemess('     |___|___|___|                        ')
		call writemess('                                          ')
	case(5)
		call writemess('                                       ')
		call writemess('      _______ _______________          ')
		call writemess('     |       |_     _|       |         ')
		call writemess('     |        _| _ |_        |         ')
		call writemess('     |__   __|__| |__|__   __|         ')
		call writemess('     |  |_| _|       |_ |_|  |        _      ')
		call writemess('     |   _ |_         _| _   |     __| |__   ')
		call writemess('     |__| |__|__   __|__| |__|   _|      _|  ')
		call writemess('     |      _|  |_| _|       |  |_      |_   ')
		call writemess('     |     |_      |_        |    |_______|  ')
		call writemess('     |_______|       |_______|               ')
		call writemess('                                       ')
		call writemess(' _______________                       ')
		call writemess('|you have bugs! |____________________________________________ ')
		call writemess('|Report ONLY bugs of the package to wang1329@mail.ustc.edu.cn|')
		call writemess('`~----------------------------------------------------------~`')
	case(6)
		call writemess('                                                             ')
		call writemess('       ___    ___    ___    ___    ___    ___                ')
		call writemess('      |___|--|___|--|___|--|___|--|___|--|___|               ')
		call writemess('        |      |      |      |      |      |                 ')
		call writemess('       _|_    _|_    _|_           _|_    _|_    _|_         ')
		call writemess('      |___|--|___|--|___|---------|___|--|___|--|___|        ')
		call writemess('                                                             ')
		call writemess(' _______________                                              ')
		call writemess('|you have bugs! |____________________________________________ ')
		call writemess('|Report ONLY bugs of the package to wang1329@mail.ustc.edu.cn|')
		call writemess('`~----------------------------------------------------------~`')
	case(7)
		call writemess('                                               ')
		call writemess('          ____             ____                ')
		call writemess('         /  \ \           / /  \               ')
		call writemess('        |###\| \         / |/###|              ')
		call writemess('        |###//  \_______/  \\###|              ')
		call writemess('         \__/               \__/               ')
		call writemess('         |                     |        ________________________________   ')
		call writemess('         \_____________________/       |you have bugs !!                |  ')
		call writemess('        / \                   / \      |Report ONLY bugs of the package |  ')
		call writemess('       /   \                 /   \     /to wang1329@mail.ustc.edu.cn !  |  ')
		call writemess('      /     \               /     \   /_________________________________|  ')
		call writemess('                                                 ')
	end select
end subroutine 

end module error