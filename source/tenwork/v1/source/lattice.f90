MODULE tensor_network
use tensor_type
use error 
use usefull_function
use string
use type_unidic
use parameter_list
implicit none

private

type site
	private
	integer::pos(2)=0
	integer,allocatable::nb_rawpos(:)
	integer,allocatable::nb_no(:)
	character(len=max_char_length)::name=''
	character(len=max_char_length),allocatable::ind(:)
	type(tensor),pointer::tensor=>Null()
	integer::nb_num=0
	logical::con_tag=.false.
	logical::exist_tag=.false.
	type(para_list)::info

	contains
	private
	!final:: clean_site
end type

type lattice
	private
	logical(kind=1)::empty_tag=.true.
	type(site),allocatable::sites(:)
	type(para_list)::info
	type(unidic)::name_dic
	integer,allocatable::raw_pos(:,:)  ! =0 if not exist 
	integer::L1=0,L2=0,max_nb_num=0,max_site_num=0
	character(len=max_char_length)::name=''
	type(tensor),allocatable::temp_ten(:)  ! used in mirror to avoid same name 
	
	contains
	private

	procedure,public:: initialize
	procedure,public:: clean
	procedure,public:: mirror_con
	procedure,public:: copy_line
	procedure,public:: get_info
	procedure,public:: draw_l
	procedure,public:: draw_tn
	generic,public:: draw=>draw_l,draw_tn
	!final:: clean_lat

	procedure,public:: add
	procedure:: delete_pos
	procedure:: delete_name
	generic,public::delete=>delete_pos,delete_name
	procedure:: move_pos
	procedure:: move_name
	generic,public::move=>move_pos,move_name
	procedure:: rename_pos
	procedure:: rename_name
	generic,public::rename=>rename_pos,rename_name

	procedure:: get_tensor_pos
	procedure:: get_tensor_name
	generic,public:: get_tensor=>get_tensor_pos,get_tensor_name
	procedure:: get_tensor_link_pos
	procedure:: get_tensor_link_name
	generic,public:: get_tensor_link=>get_tensor_link_pos,get_tensor_link_name
	procedure:: update_tensor_pos
	procedure:: update_tensor_name
	generic,public:: update_tensor=>update_tensor_pos,update_tensor_name
	procedure:: link_tensor_pos
	procedure:: link_tensor_name
	generic,public:: link_tensor=>link_tensor_pos,link_tensor_name
	procedure:: point_info_pos
	procedure:: point_info_name
	procedure:: point_info_lat
	generic,public:: point_info=>point_info_pos,point_info_name,point_info_lat

	procedure:: set_bond_name
	procedure:: set_bond_pos
	generic,public::set_bond=>set_bond_pos,set_bond_name
	procedure:: get_bond_pos
	procedure:: get_bond_name
	generic,public::get_bond=>get_bond_pos,get_bond_name
	procedure:: get_bond_no_pos
	procedure:: get_bond_no_name
	generic,public::get_bond_no=>get_bond_no_pos,get_bond_no_name

	procedure,public:: invert_bond
	procedure:: move_nb
	procedure:: remove_nb
	procedure,public:: get_nb_num

	procedure,public:: absorb_tensor
	procedure,public:: absorb
	procedure,public:: contract_type

	procedure:: get_rawpos_pos
	procedure:: get_rawpos_name
	generic:: get_rawpos=>get_rawpos_pos,get_rawpos_name
	procedure,public:: get_size
	procedure,public:: get_max_site_num
	procedure,public:: get_name_whole
	procedure,public:: get_name_site
	generic,public::get_name=>get_name_whole,get_name_site
	procedure,public:: set_name
	procedure,public:: ind_name
	procedure,public:: check_boundary
	procedure,public:: get_contag
	procedure:: set_contag_pos
	procedure:: set_contag_name
	generic,public:: set_contag=>set_contag_pos,set_contag_name
	procedure,public:: cut_bonds
	procedure,public:: max_cut_bonds
	procedure::check_exist_pos
	procedure::check_exist_name
	generic,public::check_exist=>check_exist_pos,check_exist_name
	procedure::check_exist_bond_pos
	procedure::check_exist_bond_name
	generic,public::check_exist_bond=>check_exist_bond_pos,check_exist_bond_name
	procedure,public::set_bond_as

	procedure::check_consistency
end type   

interface  check_empty
	module procedure check_empty_whole
	module procedure check_empty_site
end interface 


interface  assignment(=)
	module procedure copy_lat
end interface

public lattice, assignment(=)

contains

logical function check_exist_pos(L,pos)
implicit none
	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	
	check_exist_pos=(L%raw_pos(pos(1),pos(2))>0)
end function 


logical function check_exist_name(L,name)
implicit none
	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name
	
	check_exist_name=(L%name_dic%val(name)>0)
end function 

logical function check_exist_bond_pos(L,pos1,pos2)
implicit none
	class(lattice),intent(in) ::L
	integer,intent(in)::pos1(2),pos2(2)
	logical::exist1,exist2
	integer::rawpos1,rawpos2

	exist1=L%check_exist(pos1)
	exist2=L%check_exist(pos2)
	if(.not.(exist1 .and. exist2))then
		check_exist_bond_pos=.false.
		return
	end if
	rawpos1=L%get_rawpos(pos1)
	rawpos2=L%get_rawpos(pos2)
	check_exist_bond_pos=any(L%sites(rawpos1)%nb_rawpos(:L%sites(rawpos1)%nb_num)==rawpos2)

end function 

logical function check_exist_bond_name(L,name1,name2)
implicit none
	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name1,name2
	logical::exist1,exist2
	integer::rawpos1,rawpos2

	exist1=L%check_exist(name1)
	exist2=L%check_exist(name2)
	if(.not.(exist1 .and. exist2))then
		check_exist_bond_name=.false.
		return
	end if
	rawpos1=L%get_rawpos(name1)
	rawpos2=L%get_rawpos(name2)
	check_exist_bond_name=any(L%sites(rawpos1)%nb_rawpos(:L%sites(rawpos1)%nb_num)==rawpos2)
	
end function 

subroutine initialize(L,my_name,L1,L2,max_nb_num)
implicit none

	class(lattice),intent(inout) ::L
	integer,intent(in)::L1,L2,max_nb_num
	character(len=*),intent(in)::my_name
	
	L%L1=L1
	L%L2=L2
	L%name=my_name
	L%max_nb_num=max_nb_num
	L%max_site_num=4*L1*L2
	
	allocate(L%sites(L%max_site_num))
	allocate(L%raw_pos(2*L1,2*L2))
	L%raw_pos=0
	L%empty_tag=.false.

end subroutine

subroutine clean(L)
implicit none

	class(lattice),intent(inout) ::L
	integer::i

	if(.not. L%empty_tag)then
		L%L1=0
		L%L2=0
		L%name=''
		L%max_nb_num=0
		do i=1,L%max_site_num
			call clean_site(L%sites(i))
		end do
		deallocate(L%sites)
		deallocate(L%raw_pos)
		L%max_site_num=0
		call L%name_dic%clean()
		L%empty_tag=.true.
	end if

end subroutine

subroutine clean_lat(L)
implicit none

	type(lattice),intent(inout) ::L

	call L%clean()

end subroutine

subroutine clean_site(S)
implicit none

	type(site),intent(inout) ::S

	if(S%exist_tag)then
		S%pos=0
		deallocate(S%nb_rawpos)
		deallocate(S%nb_no)
		S%name=''
		deallocate(S%ind)
		S%tensor=>Null()
		S%nb_num=0
		S%con_tag=.false.
		S%exist_tag=.false.
	end if

end subroutine

subroutine copy_lat(L,L_old)
implicit none

	class(lattice),intent(inout)::L
	type(lattice),intent(in)::L_old
	character(len=max_char_length)::dir,dir2,name,name2
	logical::exist,exist2
	integer::i,j,nb_rawpos,nb_no

	call L%clean()
	call L%initialize(L_old%name+'_copy',L_old%L1,L_old%L2,L_old%max_nb_num)
	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			call L%add(L_old%sites(i)%pos,L_old%sites(i)%name,L_old%sites(i)%tensor)
			if(L_old%sites(i)%con_tag) call L%set_contag(L_old%sites(i)%name,.true.)
		end if
	end do
	!call L%name_dic%show()
	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			do j=1,L_old%sites(i)%nb_num
				nb_rawpos=L_old%sites(i)%nb_rawpos(j)
				nb_no=L_old%sites(i)%nb_no(j)
				dir=L_old%sites(i)%ind(j)
				dir2=L_old%sites(nb_rawpos)%ind(nb_no)
				name=L_old%sites(i)%name
				name2=L_old%sites(nb_rawpos)%name
				if(nb_rawpos>i) call L%set_bond(name,name2,dir,dir2)
			end do
		end if
	end do
			
end subroutine

subroutine set_bond_as(L,L_old)
implicit none

	class(lattice),intent(inout)::L
	type(lattice),intent(in)::L_old
	character(len=max_char_length)::dir,dir2,name,name2
	integer::i,j,nb_rawpos,nb_no

	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			do j=1,L_old%sites(i)%nb_num
				nb_rawpos=L_old%sites(i)%nb_rawpos(j)
				if(nb_rawpos>i) then
					name=L_old%sites(i)%name
					name2=L_old%sites(nb_rawpos)%name
					if(L%check_exist(name).and.L%check_exist(name2) .and. .not. L%check_exist_bond(name,name2))then
						nb_no=L_old%sites(i)%nb_no(j)
						dir=L_old%sites(i)%ind(j)
						dir2=L_old%sites(nb_rawpos)%ind(nb_no)
						call L%set_bond(name,name2,dir,dir2)
					end if
				end if
			end do
		end if
	end do
			
end subroutine

subroutine copy_line(L,line,L_old,line2,nline)
implicit none

	class(lattice),intent(inout)::L
	type(lattice),intent(in)::L_old
	integer,intent(in)::line,line2,nline
	integer::i,j,k,L1_old,L2_old,pos(2),nb_pos(2),nb_rawpos,nb_no
	character(len=max_char_length)::dir,dir2,name,name2

	call check_empty(L)
	call L_old%get_size(L1_old,L2_old)
	if(L%L2<L2_old) then
		call writemess('Lattice: '//trim(L%name)//' has smaller width than Lattice: '//trim(L_old%get_name()))
		call wc_error_stop
	end if
	if(.not.(1<=line.and.line+nline-1<=L%L1)) then
		call writemess('Input for lattice:'+L%name+'_do not statisfy 1<=line<=line+nline-1<=height')
		call wc_error_stop
	end if
	if(.not.(1<=line2.and.line2+nline-1<=L1_old)) then
		call writemess('Input for lattice:'+L_old%get_name()+'_do not statisfy 1<=line2<=line2+nline-1<=height')
		call wc_error_stop
	end if

	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			pos=L_old%sites(i)%pos
			if(pos(1)>=line2 .and. pos(1)<=line2+nline-1) then
				call L%add([pos(1)-line2+line,pos(2)],L_old%sites(i)%name,L_old%sites(i)%tensor)
				if(L_old%sites(i)%con_tag) call L%set_contag(L_old%sites(i)%name,.true.)
			end if
		end if
	end do

	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			pos=L_old%sites(i)%pos
			if(pos(1)>=line2 .and. pos(1)<=line2+nline-1) then
				do j=1,L_old%sites(i)%nb_num
					nb_rawpos=L_old%sites(i)%nb_rawpos(j)
					nb_pos=L_old%sites(nb_rawpos)%pos
					if(nb_rawpos>i .and. nb_pos(1)>=line2 .and. nb_pos(1)<=line2+nline-1) then
						nb_no=L_old%sites(i)%nb_no(j)
						dir=L_old%sites(i)%ind(j)
						dir2=L_old%sites(nb_rawpos)%ind(nb_no)
						name=L_old%sites(i)%name
						name2=L_old%sites(nb_rawpos)%name
						call L%set_bond(name,name2,dir,dir2)
					end if
				end do
			end if
		end if
	end do
			
end subroutine

subroutine mirror_con(L,L_old,line2,nline)
implicit none

	class(lattice),intent(inout)::L
	type(lattice),intent(in)::L_old
	integer,intent(in)::line2,nline
	integer::i,j,k,L1_old,L2_old,pos(2),nb_pos(2),nb_rawpos,nb_no,tn_num
	character(len=max_char_length)::name,name2,ind,ind2
	type(tensor),pointer:: tn_pointer

	call L%clean()
	call L_old%get_size(L1_old,L2_old)

	if(.not.(1<=line2.and.line2+nline-1<=L1_old)) then
		call writemess('Input for lattice:'+L_old%get_name()+'_do not statisfy 1<=line2<=line2+nline-1<=height')
		call wc_error_stop
	end if

	call L%initialize(L_old%name+'_mirror',2*L_old%L1,L_old%L2,L_old%max_nb_num)

	tn_num=0
	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			pos=L_old%sites(i)%pos
			if(pos(1)>=line2 .and. pos(1)<=line2+nline-1) then
				tn_num=tn_num+1
				call L%add([pos(1)-line2+1,pos(2)],L_old%sites(i)%name,L_old%sites(i)%tensor)
				call L%set_contag(L_old%sites(i)%name,L_old%sites(i)%con_tag)
				call L%add([line2+2*nline-pos(1),pos(2)],'mir_'+L_old%sites(i)%name)
				call L%set_contag('mir_'+L_old%sites(i)%name,.not. L_old%sites(i)%con_tag)
			end if
		end if
	end do

	allocate(L%temp_ten(tn_num))

	tn_num=0
	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			pos=L_old%sites(i)%pos
			if(pos(1)>=line2 .and. pos(1)<=line2+nline-1) then
				tn_num=tn_num+1
				L%temp_ten(tn_num)=L_old%get_tensor([pos(1),pos(2)])
				call L%link_tensor([line2+2*nline-pos(1),pos(2)],L%temp_ten(tn_num))
			end if
		end if
	end  do

	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			pos=L_old%sites(i)%pos
			if(pos(1)>=line2 .and. pos(1)<=line2+nline-1) then
				do k=1,L_old%sites(i)%nb_num
					nb_rawpos=L_old%sites(i)%nb_rawpos(k)
					nb_pos=L_old%sites(nb_rawpos)%pos
					name=L_old%sites(i)%name
					name2=L_old%sites(nb_rawpos)%name
					nb_no=L_old%sites(i)%nb_no(k)
					ind=L_old%sites(i)%ind(k)
					ind2=L_old%sites(nb_rawpos)%ind(nb_no)
					if(nb_rawpos>i .and. nb_pos(1)>=line2 .and. nb_pos(1)<=line2+nline-1) then
						call L%set_bond(name,name2,ind,ind2)
						call L%set_bond('mir_'+name,'mir_'+name2,'mir_'+ind,'mir_'+ind2)
						call L%get_tensor_link('mir_'+name,tn_pointer)
						call tn_pointer%setname(ind,'mir_'+ind)
						call L%get_tensor_link('mir_'+name2,tn_pointer)
						call tn_pointer%setname(ind2,'mir_'+ind2)
					else if (nb_pos(1)>line2+nline-1 .or. nb_pos(1)<line2) then
						call L%get_tensor_link('mir_'+name,tn_pointer)
						call tn_pointer%setname(ind,'mir_'+ind)
						call L%set_bond(name,'mir_'+name,ind,'mir_'+ind)
					end if
				end do
			end if
		end if
	end do
			
end subroutine

integer function get_rawpos_pos(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)

	get_rawpos_pos=L%raw_pos(pos(1),pos(2))
	if(get_rawpos_pos<=0)then
		call writemess(trim(str(pos))//' is empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if

end function

integer function get_rawpos_name(L,name)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name

	get_rawpos_name=L%name_dic%val(name)
	if(get_rawpos_name<=0)then
		call writemess(trim(name)//' is empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if

end function

subroutine add(L,pos,my_name_,my_tensor)
implicit none

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	character(len=*), intent(in),optional :: my_name_
	type(tensor),intent(in),optional,target::my_tensor
	character(len=max_char_length)::my_name
	integer::rawpos

	if(present(my_name_))then
		my_name=my_name_
	else
		my_name=my_tensor.Tname.1
	end if
	call check_empty(L,pos)
	call L%name_dic%add(my_name,rawpos)
	L%raw_pos(pos(1),pos(2))=rawpos
	L%sites(rawpos)%pos=pos
	if(present(my_tensor)) L%sites(rawpos)%tensor=>my_tensor
	L%sites(rawpos)%name=my_name
	L%sites(rawpos)%exist_tag=.true.
	allocate(L%sites(rawpos)%nb_rawpos(L%max_nb_num))
	allocate(L%sites(rawpos)%nb_no(L%max_nb_num))
	allocate(L%sites(rawpos)%ind(L%max_nb_num))

end subroutine

subroutine rename_pos(L,pos,new_name)
implicit none

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	character(len=*), intent(in) :: new_name
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%name_dic%rename(L%sites(rawpos)%name,new_name)
	L%sites(rawpos)%name=new_name

end subroutine

subroutine rename_name(L,old_name,new_name)
implicit none

	class(lattice),intent(inout) ::L
	character(len=*), intent(in) :: old_name,new_name
	integer::rawpos

	rawpos=L%get_rawpos(old_name)
	call L%name_dic%rename(old_name,new_name)
	L%sites(rawpos)%name=new_name

end subroutine

subroutine point_info_lat(L,para)
implicit none

	class(lattice),intent(inout),target ::L
	type(para_list),pointer, intent(inout) :: para

	para=>L%info

end subroutine

subroutine point_info_pos(L,pos,para)
implicit none

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	type(para_list),pointer, intent(inout) :: para
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	para=>L%sites(rawpos)%info

end subroutine

subroutine point_info_name(L,name,para)
implicit none

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	type(para_list),pointer, intent(inout) :: para
	integer::rawpos

	rawpos=L%get_rawpos(name)
	para=>L%sites(rawpos)%info

end subroutine

subroutine delete_pos(L,pos)
implicit none

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	integer::rawpos,nb_rawpos,i

	rawpos=L%get_rawpos(pos)
	call L%name_dic%del(L%sites(rawpos)%name)
	L%raw_pos(pos(1),pos(2))=0
	do i=1,L%sites(rawpos)%nb_num
		nb_rawpos=L%sites(rawpos)%nb_rawpos(i)
		call L%remove_nb(nb_rawpos,rawpos)
	end do
	call clean_site(L%sites(rawpos))

end subroutine

subroutine delete_name(L,name)
implicit none

	class(lattice),intent(inout) ::L
	character(len=*), intent(in) :: name
	integer::rawpos,pos(2),nb_rawpos,i

	rawpos=L%get_rawpos(name)
	call L%name_dic%del(name)
	pos=L%sites(rawpos)%pos
	L%raw_pos(pos(1),pos(2))=0
	do i=1,L%sites(rawpos)%nb_num
		nb_rawpos=L%sites(rawpos)%nb_rawpos(i)
		call L%remove_nb(nb_rawpos,rawpos)
	end do
	call clean_site(L%sites(rawpos))

end subroutine

subroutine move_pos(L,pos,new_pos)
implicit none

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2),new_pos(2)
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call check_empty(L,new_pos)
	L%raw_pos(pos(1),pos(2))=0
	L%raw_pos(new_pos(1),new_pos(2))=rawpos
	L%sites(rawpos)%pos=new_pos

end subroutine

subroutine move_name(L,name,new_pos)
implicit none

	class(lattice),intent(inout) ::L
	character(len=*),intent(in) :: name
	integer,intent(in)::new_pos(2)
	integer::rawpos,old_pos(2)

	rawpos=L%get_rawpos(name)
	call check_empty(L,new_pos)
	old_pos=L%sites(rawpos)%pos
	L%raw_pos(old_pos(1),old_pos(2))=0
	L%raw_pos(new_pos(1),new_pos(2))=rawpos
	L%sites(rawpos)%pos=new_pos

end subroutine

type(tensor) function get_tensor_pos(L,pos)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	integer::rawpos
		
	rawpos=L%get_rawpos(pos)
	get_tensor_pos=L%sites(rawpos)%tensor

end function

type(tensor) function get_tensor_name(L,name)
implicit none

	class(lattice),intent(in) ::L
	character(len=*),intent(in) :: name
	integer::rawpos
		
	rawpos=L%get_rawpos(name)
	get_tensor_name=L%sites(rawpos)%tensor

end function

subroutine get_tensor_link_pos(L,pos,tlink)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	type(tensor),pointer,intent(out)::tlink
	integer::rawpos
		
	rawpos=L%get_rawpos(pos)
	tlink=>L%sites(rawpos)%tensor

end subroutine

subroutine get_tensor_link_name(L,name,tlink)
implicit none

	class(lattice),intent(in) ::L
	character(len=*),optional,intent(in)::name
	type(tensor),pointer,intent(out)::tlink
	integer::rawpos
		
	rawpos=L%get_rawpos(name)
	tlink=>L%sites(rawpos)%tensor

end subroutine

subroutine update_tensor_pos(L,pos,new_tensor)
implicit none

	class(lattice),intent(inout) ::L
	type(tensor),intent(in)::new_tensor
	integer,intent(in)::pos(2)
	integer::rawpos
		
	rawpos=L%get_rawpos(pos)
	L%sites(rawpos)%tensor=new_tensor

end subroutine

subroutine update_tensor_name(L,name,new_tensor)
implicit none

	class(lattice),intent(inout) ::L
	type(tensor),intent(in)::new_tensor
	character(len=*),intent(in)::name
	integer::rawpos
		
	rawpos=L%get_rawpos(name)
	L%sites(rawpos)%tensor=new_tensor

end subroutine

subroutine link_tensor_pos(L,pos,my_tensor)
implicit none

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	type(tensor),intent(in),target::my_tensor
	integer::rawpos
	rawpos=L%get_rawpos(pos)
	L%sites(rawpos)%tensor=>my_tensor

end subroutine

subroutine link_tensor_name(L,name,my_tensor)
implicit none

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::name
	type(tensor),intent(in),target::my_tensor
	integer::rawpos
		
	rawpos=L%get_rawpos(name)
	L%sites(rawpos)%tensor=>my_tensor

end subroutine

subroutine set_bond_pos(L,pos,pos2,dir_,dir2_)
implicit none

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2),pos2(2)
	character(len=*),optional,intent(in)::dir_,dir2_
	character(len=max_char_length)::dir,dir2
	logical::exist,exist2
	integer::rawpos,rawpos2
	
	call check_empty(L)
	rawpos=L%get_rawpos(pos)
	rawpos2=L%get_rawpos(pos2)

	if(present(dir_))then
		dir=dir_
	else
		dir=trim(L%sites(rawpos)%name+'.'+L%sites(rawpos2)%name)
	end if

	if(present(dir2_))then
		dir2=dir2_
	else
		dir2=trim(L%sites(rawpos2)%name+'.'+L%sites(rawpos)%name)
	end if

	call set_bond_rawpos(L,rawpos,dir,rawpos2,dir2,exist,exist2)

	if(exist) call wc_error_stop('set_bond',dir+'_existed at ('+pos(1)+','+pos(2)+') for lattice_'+L%name)

	if(exist2) call wc_error_stop('set_bond',dir2+'_existed at ('+pos2(1)+','+pos2(2)+') for lattice_'+L%name)

end subroutine

subroutine set_bond_name(L,name,name2,dir_,dir2_)
implicit none

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::name,name2
	character(len=*),optional,intent(in)::dir_,dir2_
	character(len=max_char_length)::dir,dir2
	logical::exist,exist2
	integer::rawpos,rawpos2
	
	call check_empty(L)
	rawpos=L%get_rawpos(name)
	if(rawpos<=0)then
		call wc_error_stop('set_bond_name','site: '+name+' donot exist')
	end if
	rawpos2=L%get_rawpos(name2)
	if(rawpos2<=0)then
		call wc_error_stop('set_bond_name','site: '+name2+' donot exist')
	end if

	if(present(dir_))then
		dir=dir_
	else
		dir=trim(L%sites(rawpos)%name+'.'+L%sites(rawpos2)%name)
	end if

	if(present(dir2_))then
		dir2=dir2_
	else
		dir2=trim(L%sites(rawpos2)%name+'.'+L%sites(rawpos)%name)
	end if

	call set_bond_rawpos(L,rawpos,dir,rawpos2,dir2,exist,exist2)

	if(exist) call wc_error_stop('set_bond',dir+'_existed at '//trim(name)//' for lattice_'+L%name)

	if(exist2) call wc_error_stop('set_bond',dir2+'_existed at '//trim(name2)//' for lattice_'+L%name)

end subroutine

subroutine set_bond_rawpos(L,rawpos,dir,rawpos2,dir2,exist,exist2)
implicit none

	class(lattice),intent(inout) ::L
	integer,intent(in)::rawpos,rawpos2
	character(len=*),intent(in)::dir,dir2
	logical,intent(out)::exist,exist2
	integer::k,no,nb_no,nb_pos(2)

	exist=.false.
	exist2=.false.

	do k=1,L%sites(rawpos)%nb_num
		if(L%sites(rawpos)%ind(k).equ.trim(L%sites(rawpos)%name)//'.'//trim(dir))then
			exist=.true.
			return
		end if
	end do

	do k=1,L%sites(rawpos2)%nb_num
		if(L%sites(rawpos2)%ind(k).equ.trim(L%sites(rawpos2)%name)//'.'//trim(dir2))then
			exist2=.true.
			return
		end if
	end do
	
	if(L%sites(rawpos)%nb_num<L%max_nb_num)then
		L%sites(rawpos)%nb_num=L%sites(rawpos)%nb_num+1
	else
		call wc_error_stop('set_bond','site: '//trim(L%sites(rawpos)%name)//' has reach the limit of bonds number')
	end if
	if(L%sites(rawpos2)%nb_num<L%max_nb_num)then
		L%sites(rawpos2)%nb_num=L%sites(rawpos2)%nb_num+1
	else
		call wc_error_stop('set_bond','site: '//trim(L%sites(rawpos2)%name)//' has reach the limit of bonds number')
	end if

	no=L%sites(rawpos)%nb_num
	nb_no=L%sites(rawpos2)%nb_num

	L%sites(rawpos)%nb_rawpos(no)=rawpos2
	L%sites(rawpos2)%nb_rawpos(nb_no)=rawpos

	L%sites(rawpos)%ind(L%sites(rawpos)%nb_num)=dir
	L%sites(rawpos2)%ind(nb_no)=dir2

	L%sites(rawpos)%nb_no(no)=nb_no
	L%sites(rawpos2)%nb_no(nb_no)=no	

end subroutine

character(len=max_char_length) function ind_name(L,pos,no) result(ind)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2),no
	integer::rawpos
	
	call check_empty(L)
	rawpos=L%get_rawpos(pos)
	if(no>L%sites(rawpos)%nb_num) then
		call wc_error_stop('ind_name','Bond NO.'+no+' at ('+pos(1)+','+pos(2)+') does not exist.')
	end if
	ind=L%sites(rawpos)%ind(no)

end function

subroutine get_size(L,L1,L2)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(out)::L1,L2
	integer::k

	call check_empty(L)
	L1=L%L1
	L2=L%L2

end subroutine


integer function get_max_site_num(L)
implicit none

	class(lattice),intent(in) ::L

	get_max_site_num =  L%max_site_num

end function

subroutine get_bond_pos(L,pos,no,ind,nb_pos,nb_no,nb_ind)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2),nb_pos(2)
	integer,intent(out)::no,nb_no
	character(len=max_char_length),intent(out)::ind,nb_ind
	logical::existed
	integer::k,rawpos,nb_rawpos
	
	call check_empty(L)
	rawpos=L%get_rawpos(pos)
	nb_rawpos=L%get_rawpos(nb_pos)
	existed=.false.
	do k=1,L%sites(rawpos)%nb_num
		if (L%sites(rawpos)%nb_rawpos(k)==nb_rawpos) then
			no=k
			existed=.true.
			exit
		end if
	end do 

	if (existed) then
		nb_no=L%sites(rawpos)%nb_no(no)
		ind=L%sites(rawpos)%ind(no)
		nb_ind=L%sites(nb_rawpos)%ind(nb_no)
	else
		call wc_error_stop('get_bond_pos','Bond from ('+pos(1)+','+pos(2)+') to ('+nb_pos(1)+','+nb_pos(2)+') does not exist.')
	end if

end subroutine

subroutine get_bond_name(L,name,no,ind,nb_name,nb_no,nb_ind)
implicit none

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name,nb_name
	integer,intent(out)::no,nb_no
	character(len=max_char_length),intent(out)::ind,nb_ind
	logical::existed
	integer::k,rawpos,nb_rawpos
	
	call check_empty(L)
	rawpos=L%get_rawpos(name)
	nb_rawpos=L%get_rawpos(nb_name)
	existed=.false.
	do k=1,L%sites(rawpos)%nb_num
		if (L%sites(rawpos)%nb_rawpos(k)==nb_rawpos) then
			no=k
			existed=.true.
			exit
		end if
	end do 

	if (existed) then
		nb_no=L%sites(rawpos)%nb_no(no)
		ind=L%sites(rawpos)%ind(no)
		nb_ind=L%sites(nb_rawpos)%ind(nb_no)
	else
		call wc_error_stop('get_bond_pos','Bond from '+name+' to '+nb_name+' does not exist.')
	end if

end subroutine

subroutine get_bond_no_pos(L,pos,no,ind,nb_pos,nb_no,nb_ind)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2),no
	integer,intent(out)::nb_no,nb_pos(2)
	character(len=max_char_length),intent(out)::ind,nb_ind
	integer::k,rawpos,nb_rawpos
	
	call check_empty(L)
	rawpos=L%get_rawpos(pos)
	if(no>L%sites(rawpos)%nb_num) then
		call wc_error_stop('get_bond_no','Bond NO.'+no+' at ('+pos(1)+','+pos(2)+') does not exist.')
	end if

	nb_rawpos=L%sites(rawpos)%nb_rawpos(no)
	nb_no=L%sites(rawpos)%nb_no(no)
	ind=L%sites(rawpos)%ind(no)
	nb_ind=L%sites(nb_rawpos)%ind(nb_no)
	nb_pos=L%sites(nb_rawpos)%pos

end subroutine

subroutine get_bond_no_name(L,name,no,ind,nb_name,nb_no,nb_ind)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(in)::no
	character(len=*),intent(in)::name
	integer,intent(out)::nb_no
	character(len=max_char_length),intent(out)::nb_name
	character(len=max_char_length),intent(out)::ind,nb_ind
	integer::k,rawpos,nb_rawpos
	
	call check_empty(L)
	rawpos=L%get_rawpos(name)
	if(no>L%sites(rawpos)%nb_num) then
		call wc_error_stop('get_bond_no','Bond NO.'+no+' at ('+name+') does not exist.')
	end if

	nb_rawpos=L%sites(rawpos)%nb_rawpos(no)
	nb_no=L%sites(rawpos)%nb_no(no)
	ind=L%sites(rawpos)%ind(no)
	nb_ind=L%sites(nb_rawpos)%ind(nb_no)
	nb_name=L%sites(nb_rawpos)%name

end subroutine

character(len=max_char_length) function get_name_whole(L)
implicit none

	class(lattice),intent(in) ::L

	call check_empty(L)
	get_name_whole=L%name

end function

character(len=max_char_length) function get_name_site(L,pos)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	integer::rawpos


	call check_empty(L)
	rawpos=L%get_rawpos(pos)
	get_name_site=L%sites(rawpos)%name

end function

subroutine set_name(L,my_name)
implicit none

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::my_name

	call check_empty(L)
	L%name=my_name
	
end subroutine

integer function get_nb_num(L,pos)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	integer::rawpos

	call check_empty(L)
	rawpos=L%get_rawpos(pos)
	get_nb_num=L%sites(rawpos)%nb_num

end function

logical function get_contag(L,pos)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	integer::rawpos

	call check_empty(L)
	rawpos=L%get_rawpos(pos)
	get_contag=L%sites(rawpos)%con_tag

end function

subroutine set_contag_pos(L,pos,status)
implicit none

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	logical,intent(in)::status
	integer::rawpos

	call check_empty(L)
	rawpos=L%get_rawpos(pos)
	L%sites(rawpos)%con_tag=status

end subroutine

subroutine set_contag_name(L,name,status)
implicit none

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::name
	logical,intent(in)::status
	integer::rawpos

	call check_empty(L)
	rawpos=L%get_rawpos(name)
	L%sites(rawpos)%con_tag=status

end subroutine

subroutine check_empty_whole(L)
implicit none

	class(lattice),intent(in) ::L

	if(L%empty_tag) then
		call writemess('Lattice is used before generation!')
		call wc_error_stop
	end if
	
end subroutine

subroutine check_empty_site(L,pos)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)

	if(L%raw_pos(pos(1),pos(2))>0)then
		call writemess(trim(str(pos))//' is not empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if
	
end subroutine

subroutine check_boundary(L,pos)
implicit none

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)

	if(pos(1)<0.or.pos(1)>L%L1.or.pos(2)<0.or.pos(2)>L%L2) then
		call writemess('Site ('+pos(1)+','+pos(2)+') is out of boundary of a_'+L%L1+'X'+L%L2+'_lattice:'+L%name)
		call wc_error_stop
	end if
	
end subroutine

subroutine invert_bond(L,pos,temp)
implicit none

	class(lattice),intent(in)::L
	type(tensor),intent(inout)::temp
	integer,intent(in)::pos(2)
	integer::k,nb_pos(2),nb_no,rawpos,nb_rawpos

	!call temp%diminfo()
	rawpos=L%get_rawpos(pos)
	do k=1,L%sites(rawpos)%nb_num
		nb_rawpos=L%sites(rawpos)%nb_rawpos(k)
		nb_no=L%sites(rawpos)%nb_no(k)
		!write(*,*)trim(L%sites(nb_rawpos)%ind(nb_no)),trim(L%sites(rawpos)%ind(k))
		call temp%setName(L%sites(nb_rawpos)%ind(nb_no),L%sites(rawpos)%ind(k))
	end do

end subroutine

!!!!!!!!!!!!!!!!!!!!!!! Used in tn_tensor !!!!!!!!!!!!!!!!!!!!!!

subroutine absorb_tensor(L,fn_tensor,ori_tensor,pos,includes,mask)
implicit none

	class(lattice),intent(inout)::L
	type(tensor),intent(inout)::ori_tensor
	type(tensor),intent(inout)::fn_tensor
	logical(kind=1),intent(inout)::includes(:)
	logical(kind=1),intent(in),optional::mask(:)
	integer,intent(in)::pos(2)
	character(len=max_char_length)::leg(8),leg_nb(8)
	integer::k,num,nb(2),nb_no,rawpos,nb_rawpos
	type(tensor)::testen
	complex(8),pointer::tendata(:)

	call check_empty(L)
	rawpos=L%raw_pos(pos(1),pos(2))
	if(rawpos<=0) return

	if(present(mask))then
		if (mask(rawpos)) return
	end if

	if(all(.not.includes))then
		if(L%sites(rawpos)%con_tag) then
			fn_tensor=.con.L%sites(rawpos)%tensor
		else
			fn_tensor=L%sites(rawpos)%tensor
		end if
	else if(.not. includes(rawpos)) then
		num=0
		do k=1,L%sites(rawpos)%nb_num
			nb_rawpos=L%sites(rawpos)%nb_rawpos(k)
			nb_no=L%sites(rawpos)%nb_no(k)
			if(includes(nb_rawpos)) then
				num=num+1
				leg(num)=L%sites(rawpos)%ind(k)
				leg_nb(num)=L%sites(nb_rawpos)%ind(nb_no)
			end if
		end do

		if(L%sites(rawpos)%con_tag) then
			if(num==0) then
				fn_tensor=(.con.L%sites(rawpos)%tensor).kron.ori_tensor
				!call fn_tensor%diminfo
			else
				fn_tensor=contract(ori_tensor,leg_nb(1:num),.con.L%sites(rawpos)%tensor,leg(1:num))
			end if
		else
			if(num==0) then
				fn_tensor=L%sites(rawpos)%tensor.kron.ori_tensor
				!call fn_tensor%diminfo
			else
				fn_tensor=contract(ori_tensor,leg_nb(1:num),L%sites(rawpos)%tensor,leg(1:num))
			end if
		end if
	end if
	includes(rawpos)=.true.

end subroutine

subroutine contract_type(L,Tout,T1,T2,includes1,includes2)
implicit none

	class(lattice),intent(inout)::L
	type(tensor),intent(inout)::Tout,T1,T2
	logical(kind=1),intent(in)::includes1(:),includes2(:)
	character(len=max_char_length)::leg(30),leg_nb(30)
	integer::m,n,k,num,nb_no,rawpos,nb_rawpos
	type(tensor)::test_speed


	call check_empty(L)
	if(all(.not.includes1))then
		Tout=T2
	else if(all(.not.includes2))then
		Tout=T1
	else
		num=0
		do m=1,L%max_site_num
			if(includes1(m))then
				do k=1,L%sites(m)%nb_num
					nb_rawpos=L%sites(m)%nb_rawpos(k)
					if(includes2(nb_rawpos)) then
						nb_no=L%sites(m)%nb_no(k)
						num=num+1
						leg(num)=L%sites(m)%ind(k)
						leg_nb(num)=L%sites(nb_rawpos)%ind(nb_no)
					end if
				end do
			end if
		end do

		if(num==0) then
			Tout=T1.kron.T2
		else
			!write(*,*)leg(1:num)
			!write(*,*)'---------'
			!write(*,*)leg_nb(1:num)
			Tout=contract(T1,leg(1:num),T2,leg_nb(1:num))
		end if
	end if

end subroutine

integer function cut_bonds(L,i) !bond between ith and i+1th line
implicit none

	class(lattice),intent(in)::L
	integer,intent(in)::i
	integer::j,k,m,L1,L2,rawpos,nb_pos(2)

	call check_empty(L)
	call check_boundary(L,[i,1])
	cut_bonds=0
	!do j=1,i
		do k=1,L%L2
			rawpos=L%raw_pos(i,k)
			if(rawpos>0)then
				do m=1,L%sites(rawpos)%nb_num
					if(L%sites(L%sites(rawpos)%nb_rawpos(m))%pos(1)==i+1)then  
											! issue: >i or ==i+1, essential to pbc and cyl
						cut_bonds=cut_bonds+1
					end if
				end do
			end if
		end do		
	!end do	

end function 

integer function max_cut_bonds(L)
implicit none

	class(lattice),intent(in)::L
	integer::i

	call check_empty(L)
	max_cut_bonds=0
	do i=1,L%L1-1
		max_cut_bonds=max(max_cut_bonds,L%cut_bonds(i))
	end do

end function 

!!!!!!!!!!!!!!!!!!!!!!!!! Dynamic lattice !!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine remove_nb(L,rawpos,nb_rawpos)
! at rawpos, remove nb_rawpos

implicit none

	class(lattice),intent(inout) ::L
	integer,intent(in)::rawpos,nb_rawpos
	integer::i,num,nb_rawpos2,nb_no2
	integer,allocatable::move(:)

	allocate(move(L%sites(rawpos)%nb_num))

	num=0
	do i=1,L%sites(rawpos)%nb_num
		if(L%sites(rawpos)%nb_rawpos(i)/=nb_rawpos) then
			num=num+1
			if(num/=i) then
				nb_rawpos2=L%sites(rawpos)%nb_rawpos(i)
				nb_no2=L%sites(rawpos)%nb_no(i)
				L%sites(rawpos)%nb_rawpos(num)=nb_rawpos2
				L%sites(rawpos)%nb_no(num)=nb_no2
				L%sites(nb_rawpos2)%nb_no(nb_no2)=num
				L%sites(rawpos)%ind(num)=L%sites(rawpos)%ind(i)
			end if
		end if
	end do
	L%sites(rawpos)%nb_num=num
	
end subroutine

subroutine move_nb(L,rawpos,nb_rawpos1,nb_rawpos2)
! at rawpos, move bonds connected to nb_rawpos1 to nb_rawpos2

implicit none

	class(lattice),intent(inout) ::L
	integer,intent(in)::rawpos,nb_rawpos1,nb_rawpos2
	integer::i,nb_no,bond_num
	character(len=max_char_length)::old_name,new_name

	do i=1,L%sites(rawpos)%nb_num
		if(L%sites(rawpos)%nb_rawpos(i)==nb_rawpos1) then
			L%sites(nb_rawpos2)%nb_num=L%sites(nb_rawpos2)%nb_num+1
			nb_no=L%sites(nb_rawpos2)%nb_num
			L%sites(nb_rawpos2)%nb_no(nb_no)=i
			old_name=L%sites(nb_rawpos1)%ind(L%sites(rawpos)%nb_no(i))
			bond_num=count(L%sites(nb_rawpos2)%nb_rawpos==rawpos)
			new_name=trim(L%sites(rawpos)%name)//trim(str(bond_num+1))
			call L%sites(nb_rawpos2)%tensor%setName(old_name,new_name)
			L%sites(nb_rawpos2)%ind(nb_no)=new_name

			L%sites(rawpos)%nb_rawpos(i)=nb_rawpos2
			L%sites(rawpos)%nb_no(i)=nb_no
			bond_num=count(L%sites(rawpos)%nb_rawpos==nb_rawpos2)
			new_name=trim(L%sites(nb_rawpos2)%name)//trim(str(bond_num+1))
			call L%sites(rawpos)%tensor%setName(L%sites(rawpos)%ind(i),new_name)
			L%sites(rawpos)%ind(i)=new_name
		end if
	end do
	
end subroutine

subroutine absorb(L,pos,nb_pos)
implicit none

	class(lattice),intent(inout)::L
	integer,intent(in)::pos(2),nb_pos(2)
	character(len=max_char_length)::leg(8),leg_nb(8)
	integer::i,num,nb(2),nb_no,rawpos,nb_rawpos
	integer,allocatable::move(:)

	call check_empty(L)
	rawpos=L%get_rawpos(pos)
	nb_rawpos=L%get_rawpos(nb_pos)

	num=0
	do i=1,L%sites(rawpos)%nb_num
		if(L%sites(rawpos)%nb_rawpos(i)==nb_rawpos) then
			num=num+1
			nb_no=L%sites(rawpos)%nb_no(i)
			leg(num)=L%sites(rawpos)%ind(i)
			leg_nb(num)=L%sites(nb_rawpos)%ind(nb_no)
		end if
	end do

	if(L%sites(rawpos)%con_tag.neqv.L%sites(nb_rawpos)%con_tag) then
		if(num==0) then
			L%sites(rawpos)%tensor=L%sites(rawpos)%tensor.kron.(.con.L%sites(nb_rawpos)%tensor)
		else
			L%sites(rawpos)%tensor=contract(L%sites(rawpos)%tensor,leg(1:num),.con.L%sites(nb_rawpos)%tensor,leg_nb(1:num))
		end if
	else
		if(num==0) then
			L%sites(rawpos)%tensor=L%sites(rawpos)%tensor.kron.L%sites(nb_rawpos)%tensor
		else
			L%sites(rawpos)%tensor=contract(L%sites(rawpos)%tensor,leg(1:num),L%sites(nb_rawpos)%tensor,leg_nb(1:num))
		end if
	end if

	L%sites(rawpos)%nb_num=L%sites(rawpos)%nb_num-num
	call L%remove_nb(rawpos,nb_rawpos)

	do i=1,L%sites(nb_rawpos)%nb_num
		if(L%sites(nb_rawpos)%nb_rawpos(i)/=rawpos)then
			call L%move_nb(L%sites(nb_rawpos)%nb_rawpos(i),nb_rawpos,rawpos)
		end if
	end do

	L%sites(nb_rawpos)%nb_num=0

end subroutine

! subroutine QR(L,pos,new_name,bonds,unpair_inds)
! implicit none

! 	class(lattice),intent(inout)::L

! end subroutine

! subroutine SVD(L,old_name,new_name1,new_name2,bonds,unpair_inds,env)
! implicit none

! 	class(lattice),intent(inout)::L

! end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Output Info !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_info(L)
implicit none

	class(lattice),intent(in)::L
	integer::i,j,k,nb_pos,pos(2),nb_no

	if(L%empty_tag) then
		call writemess('-----------------------------------------------')
		call writemess('empty lattice')
		call writemess('-----------------------------------------------')
		call L%check_consistency()
		return
	end if

	call writemess('-----------------------------------------------')
	call writemess('name = '+L%name)
	call writemess('L1 = '+L%L1)
	call writemess('L2 = '+L%L2)
	do i=1,L%max_site_num
		if(L%sites(i)%exist_tag) then
			call writemess('---'+L%sites(i)%name)
			do k=1,L%sites(i)%nb_num
				nb_pos=L%sites(i)%nb_rawpos(k)
				nb_no=L%sites(i)%nb_no(k)
				call writemess('------>('+after_dot(L%sites(i)%ind(k))+'--'&
					+after_dot(L%sites(nb_pos)%ind(nb_no))+')'+L%sites(nb_pos)%name)
			end do
		end if
	end do

	call writemess('-----------------------------------------------')
	!call writemess('The program has been paused. Please press any key to continue')
	!read(*,*)
	call L%check_consistency()

end subroutine

subroutine draw_l(L,filename,mode)
implicit none

	class(lattice),intent(in)::L
	character(len=*),intent(in)::filename
	character(len=2),intent(in)::mode
	integer::i,j,k,nb_rawpos,nb_no,pos(2)
 	character(len=200)::command

	if(L%empty_tag) then
		call writemess('-----------------------------------------------')
		call writemess('empty lattice, do not draw anything')
		call writemess('-----------------------------------------------')
		call L%check_consistency()
		return
	end if

	open(unit=7878,file='./'//trim(filename)//'.dot')
	write(7878,*)'graph G {'
	write(7878,*) '  label = "'//trim(L%name)//'"'
	do i=1,L%max_site_num
		if(L%sites(i)%exist_tag) then
			command='  node_'+i+' ['
			if(L%sites(i)%con_tag) then
				command=command+' style = "filled,solid", fillcolor = "lemonchiffon", '
			end if
			command=command+' label = "'+L%sites(i)%name+'"'
			!write(*,*)trim(command)
			pos=L%sites(i)%pos
			if(all(pos>0))then
				if(mode(2:2)=='s')then
					command=command+', pos = "'+(3*pos(2))+','+(-pos(1))+'" ,pin="true"]'
				else
					command=command+']'
				end if
			end if
			write(7878,'(A)')trim(command)
		end if
	end do
	do i=1,L%max_site_num
		if(L%sites(i)%exist_tag) then
			do k=1,L%sites(i)%nb_num
				nb_rawpos=L%sites(i)%nb_rawpos(k)
				nb_no=L%sites(i)%nb_no(k)
				if(mode(1:1)=='e')then
					if(nb_rawpos>i )write(7878,'(A)')trim( '  node_'+i+' -- node_'+nb_rawpos)
				else if(mode=='ns')then
					if(nb_rawpos>i)write(7878,'(A)') trim('  node_'+i+' -- node_'+nb_rawpos+' [taillabel = "'+&
						after_dot(L%sites(i)%ind(k))+'",headlabel ="'+after_dot(L%sites(nb_rawpos)%ind(nb_no))+&
						'"]')
				else if(mode=='nf')then
					if(nb_rawpos>i)write(7878,'(A)') trim('  node_'+i+' -- node_'+nb_rawpos+' [taillabel = "'+&
						after_dot(L%sites(i)%ind(k))+'",headlabel ="'+after_dot(L%sites(nb_rawpos)%ind(nb_no))+&
						'",len=3]')
				end if
			end do
		end if
	end do
	write(7878,*) '}'

	!stop

	close(7878)
	!call system('neato -Teps ./'//trim(filename)//'.dot > ./'//trim(filename)//'.eps')
	!call system('evince ./'//trim(filename)//'.eps')
	call system('neato -Tpng -Gdpi=200 ./'//trim(filename)//'.dot > ./'//trim(filename)//'.png')
	call system('./show.sh ./'//trim(filename)//'.png')
	call writemess('The program has been paused. Please press enter to continue')
	read(*,*)
	call L%check_consistency()

end subroutine


subroutine draw_tn(L,filename,tnname,includes,mode)
implicit none

	class(lattice),intent(in)::L
	character(len=*),intent(in)::filename,tnname
	character(len=2),intent(in)::mode
	logical(kind=1),intent(in)::includes(:)
	character(len=200)::command
	integer::i,j,k,nb,pos(2),nb_no

	if(L%empty_tag) then
		call writemess('-----------------------------------------------')
		call writemess('empty lattice, do not draw anything')
		call writemess('-----------------------------------------------')
		call L%check_consistency()
		return
	end if

	open(unit=7878,file='./'//trim(filename)//'.dot')
	write(7878,*)'graph G {'
	write(7878,*) '  label = "'//trim(tnname)//' in '//trim(L%name)//'"'
	!write(*,*)includes
	do i=1,L%max_site_num
		if(L%sites(i)%exist_tag) then
			command='  node_'+i+' ['
			if(L%sites(i)%con_tag) then
				command=command+'style = "filled,solid", fillcolor = "lemonchiffon", '
			end if
			if(includes(i))then
				command=command+'color="red"'
			else
				command=command+'color="black"'
			end if
			command=command+', label = "'+L%sites(i)%name+'"'
			pos=L%sites(i)%pos

			if(all(pos>0))then
				if(mode(2:2)=='s')then
					command=command+', pos = "'+(3*pos(2))+','+(-pos(1))+'" ,pin="true"]'
				else
					command=command+']'
				end if
			end if
			write(7878,'(A)')trim(command)
		end if
	end do
	do i=1,L%max_site_num
		if(L%sites(i)%exist_tag) then
			do k=1,L%sites(i)%nb_num
				nb=L%sites(i)%nb_rawpos(k)
				nb_no=L%sites(i)%nb_no(k)
				if(nb>i)then
					if(mode(1:1)=='e' )then
						command='  node_'+i+' -- node_'+nb
						if(includes(i).and.includes(nb)) then
							command=command+' [color="red"]'
						end if
						write(7878,'(A)')trim(command)
					else if(mode(1:1)=='n')then
						command='  node_'+i+' -- node_'+nb+' [taillabel = "'+after_dot(L%sites(i)%ind(k))&
							+'",headlabel ="'+after_dot(L%sites(nb)%ind(nb_no))+'"'
						if(includes(i).and.includes(nb)) command=command+', color="red"'
						if(mode(2:2)=='f') command=command+', len=3'
						command=command+']'
						write(7878,'(A)')trim(command)
					end if
				end if
			end do
		end if
	end do
	write(7878,*) '}'

	close(7878)
	!call system('neato -Teps ./'//trim(filename)//'.dot > ./'//trim(filename)//'.eps')
	!call system('evince ./'//trim(filename)//'.eps')
	call system('neato -Tpng -Gdpi=200 ./'//trim(filename)//'.dot > ./'//trim(filename)//'.png')
	call system('./show.sh ./'//trim(filename)//'.png')
	call writemess('The program has been paused. Please press enter to continue')
	read(*,*)
	call L%check_consistency()

end subroutine

subroutine check_consistency(L)
implicit none

	class(lattice),intent(in)::L
	integer::num1,num2,num3
	integer::i,k,nb_rawpos,nb_no,nb_nb_rawpos,nb_nb_no
	logical::pass(4)=.true.,pass2=.true.

	call writemess('=======================================')
	call writemess('check_consistency')

	if(L%empty_tag)then

		pass2=pass2.and.(L%L1==0)
		pass2=pass2.and.(L%L2==0)
		pass2=pass2.and.(L%max_site_num==0)
		pass2=pass2.and.(L%name_dic%item_num==0)
		pass2=pass2.and.(.not.allocated(L%sites))
		pass2=pass2.and.(.not.allocated(L%raw_pos))
		if(pass2)then
			call writemess('Everything is consistent')
		else
			call writemess('Something is inconsistent for empty lattice')
		end if
	else

		pass(1)=pass(1).and. (size(L%sites)==L%max_site_num)
		pass(1)=pass(1).and. all(shape(L%raw_pos)>[L%L1,L%L2])
		if(.not.pass(1))then
			write(*,*)size(L%sites),L%max_site_num
			write(*,*)shape(L%raw_pos),[L%L1,L%L2]
			call wc_error_stop('check_consistency','size of aux arrays inconsistent!')
		end if

		num1=0
		do i=1,L%max_site_num
			if(L%sites(i)%exist_tag) num1=num1+1
		end do
		num2=count(L%raw_pos>0)
		num3=L%name_dic%item_num
		pass(2)=num1==num2 .and. num1==num3
		if(.not.pass(2))then
			write(*,*)num1,num2,num3
			call wc_error_stop('check_consistency','total number of sites inconsistent!')
		end if

		do i=1,L%max_site_num
			if(.not. site_consistent(L%sites(i)))then
				call writemess('site ('+i+') is inconsistent')
				pass(3)=.false.
			end if
		end do
		if(.not.pass(3))then
			call wc_error_stop('check_consistency','some sites are inconsistent!')
		end if

		do i=1,L%max_site_num
			if(L%sites(i)%exist_tag) then
				do k=1,L%sites(i)%nb_num
					nb_rawpos=L%sites(i)%nb_rawpos(k)
					nb_no=L%sites(i)%nb_no(k)
					if(nb_rawpos==i)then
						pass(4)=.false.
						call writemess('site ('+i+'-'+k+') links to itself')
					end if
					if(nb_rawpos>i)then
						nb_nb_rawpos=L%sites(nb_rawpos)%nb_rawpos(nb_no)
						nb_nb_no=L%sites(nb_rawpos)%nb_no(nb_no)
						if(.not.(nb_nb_rawpos==i .and. nb_nb_no==k))then
							pass(4)=.false.
							call writemess('site ('+i+'-'+k+') has wrong return')
						end if
					end if
				end do
			end if
		end do
		if(.not. pass(4))then
			call wc_error_stop('check_consistency','some bonds are inconsistent!')
		end if

		if(all(pass))then
			call writemess('Everything is consistent')
		end if
	end if
	call writemess('=======================================')

end subroutine

logical function site_consistent(S)
implicit none

	class(site),intent(in)::S
	integer::i

	site_consistent=.true.
	if(.not. S%exist_tag) then
		site_consistent=site_consistent.and.(len_trim(S%name)==0)
		site_consistent=site_consistent.and.(.not.allocated(S%ind))
		site_consistent=site_consistent.and.all(S%pos==0)
		site_consistent=site_consistent.and.(.not.S%con_tag)
		!site_consistent=site_consistent.and.(.not.associated(S%tensor))
		site_consistent=site_consistent.and.(S%nb_num==0)
		site_consistent=site_consistent.and.(.not.allocated(S%nb_rawpos))
		site_consistent=site_consistent.and.(.not.allocated(S%nb_no))
		if(.not. site_consistent)then
			write(*,*)len_trim(S%name)==0
			write(*,*).not.allocated(S%ind)
			write(*,*)all(S%pos==0)
			write(*,*).not.S%con_tag
			!write(*,*).not.associated(S%tensor)
			write(*,*)S%nb_num==0
			write(*,*).not.allocated(S%nb_rawpos)
			write(*,*).not.allocated(S%nb_no)
		end if
		return
	else
		site_consistent=site_consistent.and.(len_trim(S%name)>0)
		site_consistent=site_consistent.and.(size(S%ind)>=S%nb_num)
		site_consistent=site_consistent.and.(size(S%nb_rawpos)>=S%nb_num)
		site_consistent=site_consistent.and.(size(S%nb_no)>=S%nb_num)
		if(associated(S%tensor)) then
			do i=1,S%nb_num
				site_consistent=site_consistent.and.(S%tensor%nameorder(S%ind(i))>0)
			end do
		end if
		if(.not. site_consistent)then
			write(*,*)'position is: ',S%pos
			write(*,*)'name is',trim(S%name)
			write(*,*)'max neighbor num is: ',size(S%ind),size(S%nb_rawpos),size(S%nb_no)
			write(*,*)'neighbor num is: ',S%nb_num
			if(S%nb_num>0) then
				write(*,*)'indices are '
				do i=1,S%nb_num
					write(*,*) trim(S%ind(i))
				end do
			end if
			write(*,*)'points to a tensor ? ',associated(S%tensor)
			if(associated(S%tensor)) call S%tensor%diminfo()
		end if
		return
	end if

end function

end module tensor_network
