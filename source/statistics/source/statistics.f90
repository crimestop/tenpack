module statistics
use string
use error
use mod_mpi_info
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
		procedure::initiate
		procedure:: add_sig
		procedure:: add_ary
		generic,public::add=>add_sig,add_ary
		procedure:: get_perc_sig
		procedure:: get_perc_ary
		generic,public:: get_perc=>get_perc_sig,get_perc_ary
		procedure:: get_ave_val_sig
		procedure:: get_ave_val_ary
		generic,public:: get_ave_val=>get_ave_val_sig,get_ave_val_ary
		procedure,public::clean
		procedure,public::show
		procedure,public::show2
		procedure,public::sort_key
		procedure,public::sort_val

	end type

	public statis

contains

	subroutine initiate(ST,len)

		class(statis),intent(inout)::ST
		integer,intent(in)::len

		ST%key_len=len
		allocate(ST%key(ST%key_len,ST%max_key_num))
		allocate(ST%repeats(ST%max_key_num))
		allocate(ST%val(ST%max_key_num))
		ST%repeats=0
		ST%val=0
		ST%key=0
		ST%empty=.false.

	end subroutine

	subroutine add_sig(ST,new_key,val)

		class(statis),intent(inout)::ST
		integer,intent(in)::new_key
		real(8),optional,intent(in)::val

		call ST%add_ary([new_key],val)

	end subroutine

	subroutine add_ary(ST,new_key,val_)

		class(statis),intent(inout)::ST
		integer,intent(in)::new_key(:)
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
			call ST%initiate(size(new_key))
		end if

		if(ST%key_len/=size(new_key)) then
			call wc_error_stop('statis.add','key length not match!')
		end if

		found=.false.
		do i=1,ST%key_num
			if(all(ST%key(:,i)==new_key))then
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
			ST%key(:,ST%key_num)=new_key
		end if

	end subroutine

	real(8) function get_perc_sig(ST,new_key)
	implicit none

		class(statis),intent(inout)::ST
		integer,intent(in)::new_key

		get_perc_sig=ST%get_perc_ary([new_key])

	end function

	real(8) function get_perc_ary(ST,new_key)
	implicit none

		class(statis),intent(inout)::ST
		integer,intent(in)::new_key(:)
		integer::i
		logical(1)::found

		if(ST%empty)then
			get_perc_ary=0
			return
		end if

		if(ST%key_len/=size(new_key)) then
			call wc_error_stop('statis.get','key length not match!')
		end if

		found=.false.
		do i=1,ST%key_num
			if(all(ST%key(:,i)==new_key))then
				found=.true.
				exit
			end if
		end do

		if (found) then
			get_perc_ary=real(ST%repeats(i))/sum(ST%repeats(:ST%key_num))
		else
			get_perc_ary=0
		end if

	end function

	real(8) function get_ave_val_sig(ST,new_key)
	implicit none

		class(statis),intent(inout)::ST
		integer,intent(in)::new_key

		get_ave_val_sig=ST%get_ave_val_ary([new_key])

	end function

	real(8) function get_ave_val_ary(ST,new_key)
	implicit none

		class(statis),intent(inout)::ST
		integer,intent(in)::new_key(:)
		integer::i
		logical(1)::found

		if(ST%empty)then
			get_ave_val_ary=0
			return
		end if

		if(ST%key_len/=size(new_key)) then
			call wc_error_stop('statis.get','key length not match!')
		end if

		found=.false.
		do i=1,ST%key_num
			if(all(ST%key(:,i)==new_key))then
				found=.true.
				exit
			end if
		end do

		if (found) then
			get_ave_val_ary=ST%val(i)/sum(ST%val(:ST%key_num))
		else
			get_ave_val_ary=0
		end if

	end function

	subroutine expand(ST)

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

		class(statis),intent(inout)::ST
		integer::i,j,min_loc,temp2
		integer,allocatable::minkey(:),temp(:)
		real(8)::temp3

		if(ST%empty)then
			return
		end if

		allocate(minkey(ST%key_len))
		allocate(temp(ST%key_len))
		do i=1,ST%key_num-1 
			minkey=ST%key(:,i)
			min_loc=i
			do j=i+1,ST%key_num
				if(lessthan(ST%key(:,j),minkey))then
					minkey=ST%key(:,j)
					min_loc=j
				end if
			end do
			if(min_loc>i) call swap(ST,i,min_loc)
		end do

	end subroutine

	subroutine sort_val(ST)

		class(statis),intent(inout)::ST
		integer::i,j,min_loc,temp2
		integer,allocatable::temp(:)
		real(8)::minval,temp3


		if(ST%empty)then
			return
		end if

		allocate(temp(ST%key_len))
		do i=1,ST%key_num-1 
			minval=ST%val(i)
			min_loc=i
			do j=i+1,ST%key_num
				if(ST%val(j)<minval)then
					minval=ST%val(j)
					min_loc=j
				end if
			end do
			if(min_loc>i) call swap(ST,i,min_loc)
		end do

	end subroutine

	subroutine swap(ST,i,j)

		class(statis),intent(inout)::ST
		integer,intent(in)::i,j
		integer::temp2
		real(8)::temp3
		integer,allocatable::temp(:)

		temp=ST%key(:,i)
		ST%key(:,i)=ST%key(:,j)
		ST%key(:,j)=temp
		temp2=ST%repeats(i)
		ST%repeats(i)=ST%repeats(j)
		ST%repeats(j)=temp2
		temp3=ST%val(i)
		ST%val(i)=ST%val(j)
		ST%val(j)=temp3

	end subroutine

	logical function lessthan(a,b)

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

		class(statis),intent(inout)::ST
		integer::i,tot_rep
		real(8)::tot_val
		character(len=*)::sta_name

		if(ST%empty)then
			call write_message('Statistics on '//trim(sta_name)//' : empty')
		end if

		tot_rep=sum(ST%repeats(1:ST%key_num))
		tot_val=sum(ST%val(1:ST%key_num))

		call write_message('Statistics on '//trim(sta_name)//' :')
		do i=1,ST%key_num
			call write_message('   '//trim(sta_name)//' = '//trim(str(ST%key(:,i)))&
				//' : '//trim(str(ST%val(i)))//' '//trim(str(ST%val(i)/tot_val)))
		end do

	end subroutine

	subroutine show2(ST,sta_name)

		class(statis),intent(inout)::ST
		integer::i,tot_rep
		character(len=*)::sta_name
		real,allocatable::ave_key(:)

		if(ST%empty)then
			call write_message('Statistics on '//trim(sta_name)//' : empty')
		end if

		tot_rep=sum(ST%repeats(1:ST%key_num))
		allocate(ave_key(ST%key_len))
		do i=1,ST%key_len
			ave_key(i)=sum(ST%key(i,:ST%key_num)*ST%repeats(:ST%key_num))/real(tot_rep)
		end do

		call write_message('Statistics on '//trim(sta_name)//' :')
		do i=1,ST%key_num
			call write_message('   '//trim(sta_name)//' = '//trim(str(ST%key(:,i)))//' :'//trim(str(real(ST%repeats(i))/real(tot_rep))))
		end do
		call write_message('Average '//trim(sta_name)//' is: '//trim(str(ave_key)))

	end subroutine

	subroutine clean(ST)

		class(statis),intent(inout)::ST

		ST%key_num=1
		ST%empty=.true.
		if (allocated(ST%key)) deallocate(ST%key)
		if (allocated(ST%repeats)) deallocate(ST%repeats)
		if (allocated(ST%val)) deallocate(ST%val)

	end subroutine

end module statistics
! vi:ai:noet:sw=4 ts=4 tw=77
