module string
use error
implicit none
private

integer,parameter::max_char_length=500

interface str 
	module procedure int2str
	module procedure int_ary2str
	module procedure flt2str
	module procedure flt_ary2str
	module procedure dbl2str
	module procedure dbl_ary2str
	module procedure com2str
	module procedure com_ary2str
	module procedure dcom2str
	module procedure dcom_ary2str
	module procedure logi2str
	module procedure logi_ary2str
	module procedure str_ary2str
end interface

interface operator(//)
	module procedure concat
end interface

public max_char_length,str,operator(//),after_dot,before_dot

contains

function concat(st1,st2) result(str)

	character(:),allocatable :: str
	class(*), intent(in) :: st1,st2
	
	str=str_var(st1)//str_var(st2)

end function

function str_var(num) result(res)

	character(:),allocatable :: res
	class(*), intent(in) :: num

	select type(num)
	type is (integer)
		res=str(num)
	type is (real(4))
		res=str(num)
	type is (real(8))
		res=str(num)
	type is (complex(4))
		res=str(num)
	type is (complex(8))
		res=str(num)
	type is (character(len=*))
		res=num
	type is (logical)
		res=str(num)
	class default
		call wc_error_stop('string.str_var','input type not supported')
	end select

end function

character(len=max_char_length) function after_dot(str)

	character(len=*),intent(in)::str
	integer::pos

	pos=scan(str,'.')
	after_dot=str(pos+1:len(str))

end function

character(len=max_char_length) function before_dot(str)

	character(len=*),intent(in)::str
	integer::pos

	pos=scan(str,'.')
	before_dot=str(1:pos-1)

end function

! 1 elem

function flt2str(num,digit) result(str)

	character(:),allocatable :: str
	real(4), intent(in) :: num
	integer,intent(in),optional::digit
	
	str=dbl2str(dble(num),digit)

end function

function dbl2str(num,digit_) result(str2)

	character(:),allocatable :: str2
	character(len=25) :: str
	real(8), intent(in) :: num
	integer,intent(in),optional::digit_
	real(8) :: abs_num,num_int
	integer :: appro,tenexp,tenexp2,lenint,zero_num,digit
	integer(1) :: temp(20),i,j,dotpos,st,len_not0
	real(8) :: compare(8)=[1d-2,1d-1,1d0,1d1,1d2,1d3,1d4,1d5]

	if(present(digit_)) then
		digit=digit_
	else
		digit=7
	end if

	if (num>0d0) then
		abs_num=num
		str=''
		st=0
	else if(num<0d0) then
		abs_num=-num
		str='-'
		st=1
	else
		str2='0'
		return
	end if

	if(abs_num>compare(size(compare))*(1 - 5.1d-7) .or. abs_num<compare(1)*(1 - 5.1d-7))then
		tenexp=0
		if(abs_num>1 - 5.1d-7)then
			do while(abs_num>=10- 5.1d-6)
				tenexp=tenexp+1
				abs_num=abs_num/10
			end do
		else if (abs_num<1 - 5.1d-7)then
			do while(abs_num<1- 5.1d-7)
				tenexp=tenexp-1
				abs_num=abs_num*10
			end do
		end if

		!now 1 - 5.1d-7<=abs_num<10 - 5.1d-6

		num_int=abs_num
		do i=1,digit
			num_int=num_int*10
		end do
		appro=nint(num_int)
		do i=1,digit+1
			temp(i)=mod(appro,10)
			appro=appro/10
		end do

		str(st+1:st+1)=char(temp(digit+1)+48)
		st=st+1
		str(st+1:st+1)='.'
		do j=2,digit+1
			str(st+j:st+j)=char(temp(digit+2-j)+48)
		end do	
		st=st+digit+1

		str(st+1:st+1)='E'
		str(st+2:)=int2str(tenexp)
	else
		dotpos=count(abs_num>(compare*(1 - 5.1d-7)))
		abs_num=abs_num/compare(dotpos)
		num_int=abs_num
		do i=1,digit
			num_int=num_int*10
		end do
		appro=nint(num_int)
		dotpos=dotpos-count(0.9>compare)
		!now 1<=abs_num<10
		!dotpos = 0 if 0.1-1

		do i=1,digit+1
			temp(i)=mod(appro,10)
			appro=appro/10
		end do

		if(dotpos<=0)then
			str(st+1:st+2)='0.'
			if(dotpos<0) str(st+3:st+3-dotpos)=repeat('0',-dotpos)
			st=st+2-dotpos
			do j=1,digit+1
				str(st+j:st+j)=char(temp(digit+2-j)+48)
			end do	
		else
			do j=1,dotpos
				str(st+j:st+j)=char(temp(digit+2-j)+48)
			end do
			if(digit+1>dotpos)then
				str(st+dotpos+1:st+dotpos+1)='.'
				do j=dotpos+1,digit+1
					str(st+j+1:st+j+1)=char(temp(digit+2-j)+48)
				end do	
			end if
		end if
	end if
	str2=str(:len_trim(str))

end function

function int2str(inte) result(str)

	character(:),allocatable :: str
	integer, intent(in) :: inte
	integer :: temp2,length
	integer(1) :: temp(20),i


	if(inte==0)then
		str='0'
		return
	else if (inte>0) then
		temp2=inte
		length=0
		do while(temp2>0)
			length=length+1
			temp(length)=mod(temp2,10)
			temp2=temp2/10
		end do
		allocate(character(length) :: str)
		do i=1,length
			str(i:i)=char(temp(length+1-i)+48)
		end do
	else
		temp2=-inte
		length=0
		do while(temp2>0)
			length=length+1
			temp(length)=mod(temp2,10)
			temp2=temp2/10
		end do
		allocate(character(length+1) :: str)
		str(1:1)='-'
		do i=1,length
			str(i+1:i+1)=char(temp(length+1-i)+48)
		end do
	end if

end function

function com2str(num,digit) result(str)

	complex, intent(in) :: num
	character(:),allocatable :: str
	integer,intent(in),optional::digit

	str=dcom2str(dcmplx(num),digit)

end function

function dcom2str(num,digit) result(str)

	character(:),allocatable :: str
	complex(8), intent(in) :: num
	integer,intent(in),optional::digit

	if(aimag(num)>=0)then
		str=dbl2str(real(num),digit)//'+'//dbl2str(aimag(num),digit)//'i'
	else if(aimag(num)<=0)then
		str=dbl2str(real(num),digit)//'-'//dbl2str(-aimag(num),digit)//'i'
	else 
		str=dbl2str(real(num),digit)
	end if

end function

function logi2str(num) result(str)

	character(:),allocatable :: str
	logical, intent(in) :: num

	if(num)then
		str='T'
	else
		str='F'
	end if

end function

!array

function int_ary2str(nums,split) result(str2)

	character(:),allocatable :: str2
	character(len=max_char_length) :: str
	character(len=1),intent(in),optional::split
	integer, intent(in) :: nums(:)
	integer :: i,st,length

	if(size(nums)==0)then
		str2=''
		return
	end if
	st=0
	str=int2str(nums(1))
	st=len_trim(str)
	do i=2,size(nums)
		if(present(split))then
			str(st+1:st+1)=split
			str(st+2:st+2)=' '
			str(st+3:)=int2str(nums(i))
		else
			str(st+1:st+1)=' '
			str(st+2:)=int2str(nums(i))
		end if
		st=len_trim(str)
	end do
	str2=str(:st)

end function

function flt_ary2str(nums,split) result(str2)

	character(:),allocatable :: str2
	character(len=max_char_length) :: str
	character(len=1),intent(in),optional::split
	real(4), intent(in) :: nums(:)
	integer :: i,st

	if(size(nums)==0)then
		str2=''
	else
		st=0
		str=flt2str(nums(1))
		st=len_trim(str)
		do i=2,size(nums)
			if(present(split))then
				str(st+1:st+1)=split
				str(st+2:st+2)=' '
				str(st+3:)=flt2str(nums(i))
			else
				str(st+1:st+1)=' '
				str(st+2:)=flt2str(nums(i))
			end if
			st=len_trim(str)
		end do
		str2=str(:st)
	end if

end function

function dbl_ary2str(nums,split) result(str2)

	character(:),allocatable :: str2
	character(len=max_char_length) :: str
	character(len=1),intent(in),optional::split
	real(8), intent(in) :: nums(:)
	integer :: i,st

	if(size(nums)==0)then
		str2=''
	else
		st=0
		str=dbl2str(nums(1))
		st=len_trim(str)
		do i=2,size(nums)
			if(present(split))then
				str(st+1:st+1)=split
				str(st+2:st+2)=' '
				str(st+3:)=dbl2str(nums(i))
			else
				str(st+1:st+1)=' '
				str(st+2:)=dbl2str(nums(i))
			end if
			st=len_trim(str)
		end do
		str2=str(:st)
	end if

end function

function com_ary2str(nums) result(str2)

	character(:),allocatable :: str2
	character(len=max_char_length) :: str
	complex(4), intent(in) :: nums(:)
	integer :: i,st

	if(size(nums)==0)then
		str2=''
		return
	end if
	st=0
	str=com2str(nums(1))
	st=len_trim(str)
	do i=2,size(nums)
		str(st+1:st+2)=', '
		str(st+3:)=com2str(nums(i))
		st=len_trim(str)
	end do
	str2=str(:st)

end function

function dcom_ary2str(nums) result(str2)

	character(:),allocatable :: str2
	character(len=max_char_length) :: str
	complex(8), intent(in) :: nums(:)
	integer :: i,st

	if(size(nums)==0)then
		str2=''
		return
	end if
	st=0
	str=dcom2str(nums(1))
	st=len_trim(str)
	do i=2,size(nums)
		str(st+1:st+2)=', '
		str(st+3:)=dcom2str(nums(i))
		st=len_trim(str)
	end do
	str2=str(:st)

end function

function logi_ary2str(nums,split) result(str2)

	character(:),allocatable :: str2
	character(len=max_char_length) :: str
	logical, intent(in) :: nums(:)
	character(len=1),intent(in),optional::split
	integer :: i,st

	if(size(nums)==0)then
		str2=''
		return
	end if
	st=0
	if(nums(1))then
		str='T'
	else
		str='F'
	end if
	st=st+1
	do i=2,size(nums)
		if(present(split))then
			str(st+1:st+1)=split
			str(st+2:st+2)=' '
			if(nums(i))then
				str(st+3:st+3)='T'
			else
				str(st+3:st+3)='F'
			end if
		else
			str(st+1:st+1)=' '
			if(nums(i))then
				str(st+2:st+2)='T'
			else
				str(st+2:st+2)='F'
			end if
		end if
		st=len_trim(str)
	end do
	str2=str(:st)

end function

function str_ary2str(nums,split) result(str2)

	character(:),allocatable :: str2
	character(len=max_char_length) :: str
	character(len=*), intent(in) :: nums(:)
	character(len=1),intent(in),optional::split
	integer :: i,st

	if(size(nums)==0)then
		str2=''
		return
	end if
	st=0
	str=trim(nums(1))
	st=len_trim(str)
	do i=2,size(nums)
		if(present(split))then
			str(st+1:st+1)=split
			str(st+2:st+2)=' '
			str(st+3:)=trim(nums(i))
		else
			str(st+1:st+1)=' '
			str(st+2:)=trim(nums(i))
		end if
		st=len_trim(str)
	end do
	str2=str(:st)

end function

end module string