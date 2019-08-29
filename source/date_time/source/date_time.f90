module mod_date_time
use string
implicit none
private

public str_date_time,str_date_time_
contains

function str_date_time() result(res)

	character(:),allocatable :: res
	integer :: date_time(8)

	call date_and_time(VALUES=date_time)
	res=str(date_time(1:3),'/')//','//str(date_time(5:7),':')

end function

function str_date_time_() result(res)

	character(:),allocatable :: res
	character(len=10) :: date
	integer :: date_time(8)

	call date_and_time(DATE=date,VALUES=date_time)
	res=trim(date)//'_'//str(date_time(5:7),'_')

end function

end module