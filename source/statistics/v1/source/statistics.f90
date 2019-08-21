module statistics
use string
use error
use tools
implicit none

	private

	type statis

		private
		integer::key_num=0,max_key_num=100,key_len=1
		integer,allocatable::key(:,:),repeats(:)
		real(8),allocatable::val(:)
		logical::empty=.true.

	contains

		private
		procedure,public::initiate
		procedure:: add_num
		procedure:: add_ary
		generic,public::add=>add_num,add_ary
		procedure,public::clean
		procedure,public::show
		procedure,public::show2
		procedure,public::sort_key
		procedure,public::sort_val

	end type

	public statis

contains

	subroutine initiate(ST,len_)
	implicit none

		class(statis),intent(inout)::ST
		integer,intent(in),optional::len_

		if(present(len_))then
			ST%key_len=len_
		else
			ST%key_len=1
		end if
		allocate(ST%key(ST%key_len,ST%max_key_num))
		allocate(ST%repeats(ST%max_key_num))
		allocate(ST%val(ST%max_key_num))
		ST%repeats=0
		ST%val=0
		ST%key=0
		ST%empty=.false.

	end subroutine

	subroutine add_num(ST,new_key,val_)
	implicit none

		class(statis),intent(inout)::ST
		integer,intent(in)::new_key
		real(8),optional,intent(in)::val_
		real(8)::val
		integer::i
		logical(1)::found

		if(present(val_))then
			val=val_
		else
			val=1d0
		end if

		if(ST%empty)then
			call wc_error_stop('statis.add','please initiate the statis before use!')
		end if

		found=.false.
		do i=1,ST%key_num
			if(ST%key(1,i)==new_key)then
				found=.true.
				ST%repeats(i)=ST%repeats(i)+1
				ST%val(i)=ST%val(i)+val
				exit
			end if
		end do

		if(.not.found)then
			if(ST%key_num==ST%max_key_num)then  !exceeds boundary
				call expand(ST)
			end if
			ST%key_num=ST%key_num+1
			ST%key(1,ST%key_num)=new_key
		end if

	end subroutine

	subroutine add_ary(ST,new_key,val_)
	implicit none

		class(statis),intent(inout)::ST
		integer,intent(in)::new_key(3)
		real(8),optional,intent(in)::val_
		real(8)::val
		integer::i
		logical(1)::found

		if(present(val_))then
			val=val_
		else
			val=1d0
		end if

		if(ST%empty)then
			call wc_error_stop('statis.add','please initiate the statis before use!')
		end if

		found=.false.
		do i=1,ST%key_num
			if(all(ST%key(:size(new_key),i)==new_key))then
				found=.true.
				ST%repeats(i)=ST%repeats(i)+1
				ST%val(i)=ST%val(i)+val
				exit
			end if
		end do

		if(.not.found)then
			if(ST%key_num==ST%max_key_num)then  !exceeds boundary
				call expand(ST)
			end if
			ST%key_num=ST%key_num+1
			ST%key(:size(new_key),ST%key_num)=new_key
		end if

	end subroutine

	subroutine expand(ST)
	implicit none

		class(statis),intent(inout)::ST
		integer,allocatable::expand_temp(:),expand_temp1(:,:)
		real(8),allocatable::expand_temp2(:)

		allocate(expand_temp(ST%key_num))
		allocate(expand_temp1(ST%key_len,ST%key_num))
		allocate(expand_temp2(ST%key_num))

		expand_temp1=ST%key
		deallocate(ST%key)
		allocate(ST%key(ST%key_len,2*ST%max_key_num))
		ST%key=0
		ST%key(:,1:ST%max_key_num)=expand_temp1

		expand_temp=ST%repeats
		deallocate(ST%repeats)
		allocate(ST%repeats(2*ST%max_key_num))
		ST%repeats=0
		ST%repeats(1:ST%max_key_num)=expand_temp

		expand_temp2=ST%val
		deallocate(ST%val)
		allocate(ST%val(2*ST%max_key_num))
		ST%val=0
		ST%val(1:ST%max_key_num)=expand_temp2

		deallocate(expand_temp)
		deallocate(expand_temp1)
		deallocate(expand_temp2)
		ST%max_key_num=2*ST%max_key_num

	end subroutine


	subroutine sort_key(ST)
	implicit none

		class(statis),intent(inout)::ST
		integer::i,j,min_loc,temp2
		integer,allocatable::minkey(:),temp(:)
		real(8)::temp3

		if(ST%empty)then
			call wc_error_stop('statis.add','please initiate the statis before use!')
		end if

		allocate(minkey(ST%key_len))
		allocate(temp(ST%key_len))
		do i=1,ST%key_num-1  !sort key
			minkey=ST%key(:,i)
			min_loc=i
			do j=i+1,ST%key_num
				if(lessthan(ST%key(:,j),minkey))then
					minkey=ST%key(:,j)
					min_loc=j
				end if
			end do
			if(min_loc>i)then
				temp=ST%key(:,i)
				ST%key(:,i)=ST%key(:,min_loc)
				ST%key(:,min_loc)=temp
				temp2=ST%repeats(i)
				ST%repeats(i)=ST%repeats(min_loc)
				ST%repeats(min_loc)=temp2
				temp3=ST%val(i)
				ST%val(i)=ST%val(min_loc)
				ST%val(min_loc)=temp3
			end if
		end do

	end subroutine

	subroutine sort_val(ST)
	implicit none

		class(statis),intent(inout)::ST
		integer::i,j,min_loc,temp2
		integer,allocatable::temp(:)
		real(8)::minval,temp3


		if(ST%empty)then
			call wc_error_stop('statis.add','please initiate the statis before use!')
		end if

		allocate(temp(ST%key_len))
		do i=1,ST%key_num-1  !sort key
			minval=ST%val(i)
			min_loc=i
			do j=i+1,ST%key_num
				if(ST%val(j)<minval)then
					minval=ST%val(j)
					min_loc=j
				end if
			end do
			if(min_loc>i)then
				temp=ST%key(:,i)
				ST%key(:,i)=ST%key(:,min_loc)
				ST%key(:,min_loc)=temp
				temp2=ST%repeats(i)
				ST%repeats(i)=ST%repeats(min_loc)
				ST%repeats(min_loc)=temp2
				temp3=ST%val(i)
				ST%val(i)=ST%val(min_loc)
				ST%val(min_loc)=temp3
			end if
		end do

	end subroutine

	logical function lessthan(a,b)
	implicit none

		integer,intent(in)::a(:),b(:)
		integer::i 

		if(size(a)/=size(b))then
			call wc_error_stop('statistics.lessthan','input arrays have different length')
		end if

		do i=1,size(a)
			if(a(i)<b(i))then
				lessthan=.true.
				return
			else if(a(i)>b(i))then
				lessthan=.false.
				return
			end if
		end do
		lessthan=.false.
		return

	end function

	subroutine show(ST,sta_name)
	implicit none

		class(statis),intent(inout)::ST
		integer::i,tot_rep
		real(8)::tot_val
		character(len=*)::sta_name


		if(ST%empty)then
			call wc_error_stop('statis.add','please initiate the statis before use!')
		end if

		tot_rep=sum(ST%repeats(1:ST%key_num))
		tot_val=sum(ST%val(1:ST%key_num))

		call writemess('Statistics on '//trim(sta_name)//' :')
		do i=1,ST%key_num
			call writemess('   '//trim(sta_name)//' = '//trim(str(ST%key(:,i)))&
				//' : '//trim(str(ST%val(i)))//' '//trim(str(ST%val(i)/tot_val)))
		end do


	end subroutine

	subroutine show2(ST,sta_name)
	implicit none

		class(statis),intent(inout)::ST
		integer::i,tot_rep
		character(len=*)::sta_name
		real,allocatable::ave_key(:)


		if(ST%empty)then
			call wc_error_stop('statis.add','please initiate the statis before use!')
		end if

		tot_rep=sum(ST%repeats(1:ST%key_num))
		allocate(ave_key(ST%key_len))
		do i=1,ST%key_len
			ave_key(i)=sum(ST%key(i,:ST%key_num)*ST%repeats(:ST%key_num))/real(tot_rep)
		end do

		call writemess('Statistics on '//trim(sta_name)//' :')
		do i=1,ST%key_num
			call writemess('   '//trim(sta_name)//' = '//trim(str(ST%key(:,i)))//' :'//trim(str(real(ST%repeats(i))/real(tot_rep))))
		end do
		call writemess('Average '//trim(sta_name)//' is: '//trim(str(ave_key)))


	end subroutine

	subroutine clean(ST)
	implicit none

		class(statis),intent(inout)::ST

		ST%key_num=0
		ST%repeats=0
		ST%val=0

	end subroutine

end module statistics
! vi:ai:noet:sw=4 ts=4 tw=77
