module string

implicit none

integer,parameter::max_char_length=500
integer,parameter::max_char_length_short=25

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

contains

character(len=max_char_length) function after_dot(str)
implicit none

	character(len=*),intent(in)::str
	integer::pos

	pos=scan(str,'.')
	after_dot=str(pos+1:len(str))

end function

character(len=max_char_length_short) function flt2str(num,length) result(str)
implicit none

	real(4), intent(in) :: num
	integer,intent(out),optional::length
	
	str=dbl2str(dble(num),length)

end function

character(len=max_char_length_short) function dbl2str(num,length) result(str)
implicit none

	real(8), intent(in) :: num
	integer,intent(out),optional::length
	real(8) :: temp2
	integer :: appro,tenexp,tenexp2,lenint
	integer(1) :: temp(10),i,j,k,dotpos,st,len_not0
	real(8) :: compare(8)=[1d-2,1d-1,1d0,1d1,1d2,1d3,1d4,1d5]

	if (num>0d0) then
		temp2=num*(1+1d-10)
		str=''
		st=0
	else if(num<0d0) then
		temp2=-num*(1+1d-10)
		str='-'
		st=1
	else
		str='0'
		if(present(length)) length=1
		return
	end if

	if(temp2>compare(size(compare)) .or. temp2<compare(1))then
		tenexp=0
		if(temp2>1)then
			do while(temp2>=10)
				tenexp=tenexp+1
				temp2=temp2/10
			end do
		else if (temp2<1)then
			do while(temp2<1)
				tenexp=tenexp-1
				temp2=temp2*10
			end do
		end if

		appro=nint(temp2*1d5)
		i=0
		do while(appro>0)
			i=i+1
			temp(i)=mod(appro,10)
			appro=appro/10
		end do

		k=0
		do while(temp(k+1)==0)
			k=k+1
		end do

		str(st+1:st+1)=char(temp(i)+48)
		st=st+1
		if(i-k>=2)then
			str(st+1:st+1)='.'
			do j=2,i-k
				str(st+j:st+j)=char(temp(i+1-j)+48)
			end do	
			st=st+i-k
		end if
! 		if(tenexp>0)then
! 			str(st+i-k+2:st+i-k+3)='E+'
! 			str(st+i-k+4:)=int2str(tenexp,lenint)
! 			if(present(length)) length=st+i-k+3+lenint
! 		else
			str(st+1:st+1)='E'
			str(st+2:)=int2str(tenexp,lenint)
			if(present(length)) length=st+1+lenint
!		end if
	else
		dotpos=count(temp2>compare)
		appro=nint(temp2/compare(dotpos)*1d5)
		dotpos=dotpos-count(0.9>compare)

		i=0
		do while(appro>0)
			i=i+1
			temp(i)=mod(appro,10)
			appro=appro/10
		end do
		k=0
		do while(temp(k+1)==0)
			k=k+1
		end do

		if(dotpos<=0)then
			str(st+1:st+2)='0.'
			if(dotpos<0) str(st+3:st+4-dotpos)=repeat('0',-dotpos)
			st=st+2-dotpos
			do j=1,i-k
				str(st+j:st+j)=char(temp(i+1-j)+48)
			end do	
			if(present(length)) length=st+i-k
		else
			do j=1,dotpos
				str(st+j:st+j)=char(temp(i+1-j)+48)
			end do
			if(i-k>dotpos)then
				str(st+dotpos+1:st+dotpos+1)='.'
				do j=dotpos+1,i-k
					str(st+j+1:st+j+1)=char(temp(i+1-j)+48)
				end do	
				if(present(length)) length=st+i-k+1
			else
				if(present(length)) length=st+dotpos
			end if
		end if
	end if

end function

character(len=max_char_length_short) function int2str(inte,length) result(str)
implicit none

	integer, intent(in) :: inte
	integer,intent(out),optional::length
	integer :: temp2
	integer(1) :: temp(20),i,j,st

	if(inte==0)then
		str='0'
		if(present(length)) length=1
		return
	else if (inte>0) then
		temp2=inte
		str=''
		st=0
	else
		temp2=-inte
		str='-'
		st=1
	end if

	i=0
	do while(temp2>0)
		i=i+1
		temp(i)=mod(temp2,10)
		temp2=temp2/10
	end do
	do j=1,i
		str(st+j:st+j)=char(temp(i+1-j)+48)
	end do

	if(present(length)) length=st+i

end function

character(len=max_char_length) function int_ary2str(nums) result(str)
implicit none

	integer, intent(in) :: nums(:)
	integer :: i,st,length

	if(size(nums)==0)then
		str=''
		return
	end if
	st=0
	str=int2str(nums(1),length)
	st=st+length
	do i=2,size(nums)
		str(st+1:st+2)=', '
		str(st+3:)=int2str(nums(i),length)
		st=st+length+2
	end do

end function

character(len=max_char_length) function flt_ary2str(nums) result(str)
implicit none

	real(4), intent(in) :: nums(:)
	integer :: i,st,length

	if(size(nums)==0)then
		str=''
		return
	end if
	st=0
	str=flt2str(nums(1),length)
	st=st+length
	do i=2,size(nums)
		str(st+1:st+2)=', '
		str(st+3:)=flt2str(nums(i),length)
		st=st+length+2
	end do

end function

character(len=max_char_length) function dbl_ary2str(nums) result(str)
implicit none

	real(8), intent(in) :: nums(:)
	integer :: i,st,length

	if(size(nums)==0)then
		str=''
		return
	end if
	st=0
	str=dbl2str(nums(1),length)
	st=st+length
	do i=2,size(nums)
		str(st+1:st+2)=', '
		str(st+3:)=dbl2str(nums(i),length)
		st=st+length+2
	end do

end function

character(len=max_char_length) function com2str(num,length) result(str)
implicit none

	complex(4), intent(in) :: num
	integer,intent(out),optional::length
	integer :: i,st,lenn

	st=0
	str=flt2str(real(num),lenn)
	st=st+lenn
	if(aimag(num)>=0)then
		str(st+1:st+1)='+'
		st=st+1
	end if
	str(st+1:)=flt2str(aimag(num),lenn)
	st=st+lenn
	str(st+1:st+1)='i'
	if(present(length))length=st+1

end function

character(len=max_char_length) function dcom2str(num,length) result(str)
implicit none

	complex(8), intent(in) :: num
	integer,intent(out),optional::length
	integer :: i,st,lenn

	st=0
	str=dbl2str(real(num),lenn)
	st=st+lenn
	if(dimag(num)>=0)then
		str(st+1:st+1)='+'
		st=st+1
	end if
	str(st+1:)=dbl2str(aimag(num),lenn)
	st=st+lenn
	str(st+1:st+1)='i'
	if(present(length))length=st+1

end function

character(len=max_char_length) function com_ary2str(nums) result(str)
implicit none

	complex(4), intent(in) :: nums(:)
	integer :: i,st,length

	if(size(nums)==0)then
		str=''
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

end function

character(len=max_char_length) function dcom_ary2str(nums) result(str)
implicit none

	complex(8), intent(in) :: nums(:)
	integer :: i,st,length

	if(size(nums)==0)then
		str=''
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

end function

character(len=max_char_length) function logi_ary2str(nums) result(str)
implicit none

	logical, intent(in) :: nums(:)
	integer :: i,st,length

	if(size(nums)==0)then
		str=''
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
		str(st+1:st+2)=', '
		if(nums(i))then
			str(st+3:st+3)='T'
		else
			str(st+3:st+3)='F'
		end if
		st=st+3
	end do

end function

character(len=max_char_length_short) function logi2str(num) result(str)
implicit none

	logical, intent(in) :: num

	if(num)then
		str='T'
	else
		str='F'
	end if

end function

end module string