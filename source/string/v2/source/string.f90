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
end interface

interface operator(//)
	module procedure concat
end interface

public max_char_length,str,operator(//),after_dot

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

function flt2str(num,length) result(str)

	character(:),allocatable :: str
	real(4), intent(in) :: num
	integer,intent(out),optional::length
	
	str=dbl2str(dble(num),length)

end function

function dbl2str(num,length) result(str2)

	character(:),allocatable :: str2
	character(len=25) :: str
	real(8), intent(in) :: num
	integer,intent(out),optional::length
	real(8) :: abs_num
	integer :: appro,tenexp,tenexp2,lenint,zero_num
	integer(1) :: temp(10),i,j,dotpos,st,len_not0
	real(8) :: compare(8)=[1d-2,1d-1,1d0,1d1,1d2,1d3,1d4,1d5]

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
		if(present(length)) length=1
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

		appro=nint(abs_num*1d5)
		do i=1,6
			temp(i)=mod(appro,10)
			appro=appro/10
		end do

		zero_num=0
		do while(temp(zero_num+1)==0)
			zero_num=zero_num+1
		end do

		str(st+1:st+1)=char(temp(6)+48)
		st=st+1
		if(zero_num<5)then
			str(st+1:st+1)='.'
			do j=2,6-zero_num
				str(st+j:st+j)=char(temp(7-j)+48)
			end do	
			st=st+6-zero_num
		end if
		str(st+1:st+1)='E'
		str(st+2:)=int2str(tenexp,lenint)
		if(present(length)) length=st+1+lenint
		str2=str(:st+1+lenint)
	else
		dotpos=count(abs_num>(compare*(1 - 5.1d-7)))
		abs_num=abs_num/compare(dotpos)
		appro=nint(abs_num*1d5)
		dotpos=dotpos-count(0.9>compare)
		!now 1<=abs_num<10
		!dotpos = 0 if 0.1-1

		do i=1,6
			temp(i)=mod(appro,10)
			appro=appro/10
		end do
		zero_num=0
		do while(temp(zero_num+1)==0)
			zero_num=zero_num+1
		end do

		if(dotpos<=0)then
			str(st+1:st+2)='0.'
			if(dotpos<0) str(st+3:st+3-dotpos)=repeat('0',-dotpos)
			st=st+2-dotpos
			do j=1,6-zero_num
				str(st+j:st+j)=char(temp(7-j)+48)
			end do	
			if(present(length)) length=st+6-zero_num
			str2=str(:st+6-zero_num)
		else
			do j=1,dotpos
				str(st+j:st+j)=char(temp(7-j)+48)
			end do
			if(6-zero_num>dotpos)then
				str(st+dotpos+1:st+dotpos+1)='.'
				do j=dotpos+1,6-zero_num
					str(st+j+1:st+j+1)=char(temp(7-j)+48)
				end do	
				if(present(length)) length=st+7-zero_num
				str2=str(:st+7-zero_num)
			else
				if(present(length)) length=st+dotpos
				str2=str(:st+dotpos)
			end if
		end if
	end if

end function

function int2str(inte,length_) result(str)

	character(:),allocatable :: str
	integer, intent(in) :: inte
	integer,intent(out),optional::length_
	integer :: temp2
	integer(1) :: temp(20),length,i


	if(inte==0)then
		str='0'
		if(present(length_)) length_=1
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
		if(present(length_)) length_=length
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
		if(present(length_)) length_=length+1
	end if

end function

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
	str=int2str(nums(1),length)
	st=st+length
	do i=2,size(nums)
		if(present(split))then
			str(st+1:st+1)=split
			str(st+2:st+2)=' '
			str(st+3:)=int2str(nums(i),length)
			st=st+length+2
		else
			str(st+1:st+1)=' '
			str(st+2:)=int2str(nums(i),length)
			st=st+length+1
		end if
	end do
	str2=str(:st)

end function

function flt_ary2str(nums,split) result(str2)

	character(:),allocatable :: str2
	character(len=max_char_length) :: str
	character(len=1),intent(in),optional::split
	real(4), intent(in) :: nums(:)
	integer :: i,st,length

	if(size(nums)==0)then
		str2=''
		return
	end if
	st=0
	str=flt2str(nums(1),length)
	st=st+length
	do i=2,size(nums)
		if(present(split))then
			str(st+1:st+1)=split
			str(st+2:st+2)=' '
			str(st+3:)=flt2str(nums(i),length)
			st=st+length+2
		else
			str(st+1:st+1)=' '
			str(st+2:)=flt2str(nums(i),length)
			st=st+length+1
		end if
	end do
	str2=str(:st)

end function

function dbl_ary2str(nums,split) result(str2)

	character(:),allocatable :: str2
	character(len=max_char_length) :: str
	character(len=1),intent(in),optional::split
	real(8), intent(in) :: nums(:)
	integer :: i,st,length

	if(size(nums)==0)then
		str2=''
		return
	end if
	st=0
	str=dbl2str(nums(1),length)
	st=st+length
	do i=2,size(nums)
		if(present(split))then
			str(st+1:st+1)=split
			str(st+2:st+2)=' '
			str(st+3:)=dbl2str(nums(i),length)
			st=st+length+2
		else
			str(st+1:st+1)=' '
			str(st+2:)=dbl2str(nums(i),length)
			st=st+length+1
		end if
	end do
	str2=str(:st)

end function

function com2str(num,length) result(str)

	character(:),allocatable :: str
	complex(4), intent(in) :: num
	integer,intent(out),optional::length
	integer :: len1,len2

	if(aimag(num)>=0)then
		str=flt2str(real(num),len1)//'+'//flt2str(aimag(num),len2)//'i'
		if(present(length))length=len1+len2+2
	else if(aimag(num)<=0)then
		str=flt2str(real(num),len1)//'-'//flt2str(-aimag(num),len2)//'i'
		if(present(length))length=len1+len2+2
	else 
		str=flt2str(real(num),len1)
		if(present(length))length=len1
	end if

end function

function dcom2str(num,length) result(str)

	character(:),allocatable :: str
	complex(8), intent(in) :: num
	integer,intent(out),optional::length
	integer :: len1,len2

	if(aimag(num)>=0)then
		str=dbl2str(real(num),len1)//'+'//dbl2str(aimag(num),len2)//'i'
		if(present(length))length=len1+len2+2
	else if(aimag(num)<=0)then
		str=dbl2str(real(num),len1)//'-'//dbl2str(-aimag(num),len2)//'i'
		if(present(length))length=len1+len2+2
	else 
		str=dbl2str(real(num),len1)
		if(present(length))length=len1
	end if

end function

function com_ary2str(nums) result(str2)

	character(:),allocatable :: str2
	character(len=max_char_length) :: str
	complex(4), intent(in) :: nums(:)
	integer :: i,st,length

	if(size(nums)==0)then
		str2=''
		return
	end if
	st=0
	str=com2str(nums(1),length)
	st=st+length
	do i=2,size(nums)
		str(st+1:st+2)=', '
		str(st+3:)=com2str(nums(i),length)
		st=st+length+2
	end do
	str2=str(:st)

end function

function dcom_ary2str(nums) result(str2)

	character(:),allocatable :: str2
	character(len=max_char_length) :: str
	complex(8), intent(in) :: nums(:)
	integer :: i,st,length

	if(size(nums)==0)then
		str2=''
		return
	end if
	st=0
	str=dcom2str(nums(1),length)
	st=st+length
	do i=2,size(nums)
		str(st+1:st+2)=', '
		str(st+3:)=dcom2str(nums(i),length)
		st=st+length+2
	end do
	str2=str(:st)

end function

function logi_ary2str(nums) result(str)

	character(:),allocatable :: str
	logical, intent(in) :: nums(:)
	integer :: i,st,length

	allocate(character(min(2*size(nums)-1,0)) :: str)
	st=0
	if(nums(1))then
		str='T'
	else
		str='F'
	end if
	st=st+1
	do i=2,size(nums)
		str(st+1:st+2)=', '
		if(nums(i))then
			str(st+3:st+3)='T'
		else
			str(st+3:st+3)='F'
		end if
		st=st+3
	end do

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

end module string