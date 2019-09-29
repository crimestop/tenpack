module error
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

	call write_message('In '//trim(error_pos)//':')
	call write_message(error_mess)
	call wc_error_stop_nomess()

end subroutine

subroutine wc_error_stop_mess1(error_mess)

	character(len=*),intent(in)::error_mess
	integer::ierr

	call write_message(error_mess)
	call wc_error_stop_nomess()

end subroutine	

subroutine wc_error_stop_nomess()

	integer::ierr
	integer,pointer::t
	
	call wc_outpicture()
	if(.not. test_tag)then
		t=0
		if(nproc>1) call MPI_Finalize(ierr)
		stop
	end if

end subroutine	

subroutine wc_outpicture()

	integer::pic

	call system_clock(pic) ! millisecond

	select case(modulo(pic,7)+1)
	case(1)
		call write_message('                                                 ')
		call write_message('      __                                         ')
		call write_message('     #####                    ###                ')
		call write_message('    #########___---------____#####               ')
		call write_message('    ########                 #####               ')
		call write_message('     ######                   ###                ')
		call write_message('      ####                     ##                ')
		call write_message('       ##                       \     ________________________________   ')
		call write_message('       /        ##       ##      |   |you have bugs!!                 |  ')
		call write_message('       |       #0#       #0#      |  |Report ONLY bugs of the package |  ')
		call write_message('       |      ###         ###     |  /to wang1329@mail.ustc.edu.cn !  |  ')
		call write_message('       |      #            ##     | /_________________________________|  ')
		call write_message('       |             ####         |              ')
		call write_message('      /\         \    ##   /     /               ')
		call write_message('     /  \         \_______/     /                ')
		call write_message('    ###  \              \\     ##                ')
		call write_message('   ######################\\########              ')
		call write_message(' #########################\\########             ')
		call write_message('                           \\                    ')
		call write_message('                                                 ')
	case(2)
		call write_message('                                                ')
		call write_message('                      ________                  ')
		call write_message('                    _/__|__|__\_                ')
		call write_message('                   /     _      \               ')
		call write_message('                  /_   _(_)_   __\              ')
		call write_message('                 ||_| |____o| |_|_|             ')
		call write_message('             ____|================|___          ')
		call write_message('            |    |   __________   |   \         ')
		call write_message('            |    |  | WARNING! |  |    \        ')
		call write_message('            |   ||  | you      |  |\    \       ')
		call write_message('            |   ||  |  have    |  | \    \      ')
		call write_message('            |   ||  |    bugs  |  |  \    \     ')
		call write_message('            |   ||  |__________|  |   \    \    ')
		call write_message('            |   ||                |    \____\   ')
		call write_message('            |   ||________________|      | \    ')
		call write_message('            |___|      \     /           |  \   ')
		call write_message('             / \        \___/            |___\  ')
		call write_message('            /   \       /   \                   ')
		call write_message('           /_____\     /_____\                  ')
		call write_message('_____________________________________________________________ ')
		call write_message('|Report ONLY bugs of the package to wang1329@mail.ustc.edu.cn|')
		call write_message('`~----------------------------------------------------------~`')
	case(3)
		call write_message('                                  ')
		call write_message('                    |_|           ')
		call write_message('                  _P   P_         ')
		call write_message('           \___  /  \|/  \  ___/  ')
		call write_message('               \/    |    \/      ')
		call write_message('                |  0 |   0|       ')
		call write_message('             __/| 000|  00|\__    ')
		call write_message('            /   \  00|    /   \   ')
		call write_message('              __/\___|___/\__     ')
		call write_message('             /               \    ')
		call write_message(' _______________                  ')
		call write_message('|you have bugs! |____________________________________________ ')
		call write_message('|Report ONLY bugs of the package to wang1329@mail.ustc.edu.cn|')
		call write_message('`~----------------------------------------------------------~`')
	case(4)
		call write_message('                                          ')
		call write_message('                ______                    ')
		call write_message('               /__    \                   ')
		call write_message('              /\_/     \                  ')
		call write_message('            _/__       |                  ')
		call write_message('           / \_/       |                  ')
		call write_message('      _____|_____      |               ________________________________ ')
		call write_message('     |   |   |   |     |              |you have bugs!!                 |')
		call write_message('     |   |   |   |     |              |Report ONLY bugs of the package |')
		call write_message('     |   |   |   |     /              |to wang1329@mail.ustc.edu.cn !  |')
		call write_message('     |___|___|___|_   /              /`-------------------------------- ')
		call write_message('    /  _  _   _    \_/             ()     ')
		call write_message('    | | || |.| | | |            --/|_/    ')
		call write_message('    | |_||_|.|_| | |            --\/      ')
		call write_message('    \______________/            --/-\     ')
		call write_message('     |   |   |   |               /  /     ')
		call write_message('     |   |   |   |                        ')
		call write_message('     |   |   |   |                        ')
		call write_message('     |   |   |   |                        ')
		call write_message('     |   |   |   |                        ')
		call write_message('     |___|___|___|                        ')
		call write_message('                                          ')
	case(5)
		call write_message('                                       ')
		call write_message('      _______ _______________          ')
		call write_message('     |       |_     _|       |         ')
		call write_message('     |        _| _ |_        |         ')
		call write_message('     |__   __|__| |__|__   __|         ')
		call write_message('     |  |_| _|       |_ |_|  |        _      ')
		call write_message('     |   _ |_         _| _   |     __| |__   ')
		call write_message('     |__| |__|__   __|__| |__|   _|      _|  ')
		call write_message('     |      _|  |_| _|       |  |_      |_   ')
		call write_message('     |     |_      |_        |    |_______|  ')
		call write_message('     |_______|       |_______|               ')
		call write_message('                                       ')
		call write_message(' _______________                       ')
		call write_message('|you have bugs! |____________________________________________ ')
		call write_message('|Report ONLY bugs of the package to wang1329@mail.ustc.edu.cn|')
		call write_message('`~----------------------------------------------------------~`')
	case(6)
		call write_message('                                                             ')
		call write_message('       ___    ___    ___    ___    ___    ___                ')
		call write_message('      |___|--|___|--|___|--|___|--|___|--|___|               ')
		call write_message('        |      |      |      |      |      |                 ')
		call write_message('       _|_    _|_    _|_           _|_    _|_    _|_         ')
		call write_message('      |___|--|___|--|___|---------|___|--|___|--|___|        ')
		call write_message('                                                             ')
		call write_message(' _______________                                              ')
		call write_message('|you have bugs! |____________________________________________ ')
		call write_message('|Report ONLY bugs of the package to wang1329@mail.ustc.edu.cn|')
		call write_message('`~----------------------------------------------------------~`')
	case(7)
		call write_message('                                               ')
		call write_message('          ____             ____                ')
		call write_message('         /  \ \           / /  \               ')
		call write_message('        |###\| \         / |/###|              ')
		call write_message('        |###//  \_______/  \\###|              ')
		call write_message('         \__/               \__/               ')
		call write_message('         |                     |        ________________________________   ')
		call write_message('         \_____________________/       |you have bugs !!                |  ')
		call write_message('        / \                   / \      |Report ONLY bugs of the package |  ')
		call write_message('       /   \                 /   \     /to wang1329@mail.ustc.edu.cn !  |  ')
		call write_message('      /     \               /     \   /_________________________________|  ')
		call write_message('                                                 ')
	end select
end subroutine 

end module error