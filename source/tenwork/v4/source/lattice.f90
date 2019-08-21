MODULE tensor_network
use tensor_type
use error 
use usefull_function
use string
use type_unidic
use mod_dictionary
implicit none

private

type bond
	private
	integer::nb_rawpos
	integer::nb_no
	logical:: env_tag=.false.
	type(tensor)::env
	character(len=max_char_length)::ind=''
end type

type site
	private
	integer::pos(2)=0
	type(bond),allocatable::bonds(:)
	character(len=max_char_length)::name=''
	type(tensor),pointer::tensor=>Null()
	type(tensor)::tensor_save
	integer::nb_num=0
	logical::tensor_save_tag=.false.
	logical::con_tag=.false.
	logical::exist_tag=.false.
	type(dictionary)::info

	contains
	private
	!final:: clean_site

end type

type lattice
	private
	logical(kind=1)::empty_tag=.true.
	type(site),allocatable::sites(:)
	type(dictionary)::info
	type(unidic)::name_dic
	integer,allocatable::raw_pos(:,:)  ! =0 if not exist 
	integer::L1=0,L2=0,max_nb_num=0,max_site_num=0
	character(len=max_char_length)::name=''
	
	contains
	private

	procedure,public:: initialize
	procedure,public:: clean

	procedure,public:: mirror_con
	procedure,public:: copy_line
	procedure,public::set_bond_as

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
	procedure:: get_env_link_pos
	procedure:: get_env_link_name
	generic,public:: get_env_link=>get_env_link_pos,get_env_link_name
	procedure:: update_tensor_pos
	procedure:: update_tensor_name
	generic,public:: update_tensor=>update_tensor_pos,update_tensor_name
	procedure:: set_tensor_pos
	procedure:: set_tensor_name
	generic,public:: set_tensor=>set_tensor_pos,set_tensor_name
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
	procedure::bonds_num_pos
	procedure::bonds_num_name
	procedure::bonds_num_rawpos
	generic,public::bonds_num=>bonds_num_pos,bonds_num_name
	procedure::check_empty_whole
	procedure::check_empty_site_pos
	procedure::check_empty_site_name
	generic,public::check_empty=>check_empty_whole,check_empty_site_pos,check_empty_site_name
	procedure::check_unempty_whole
	procedure::check_unempty_site_pos
	procedure::check_unempty_site_name
	generic,public::check_unempty=>check_unempty_whole,check_unempty_site_pos,check_unempty_site_name

	procedure::check_consistency

	procedure,public::write
	procedure,public::read

	procedure,public:: generate_ten
	procedure,public:: generate_env
	procedure:: absorb_env_whole
	procedure:: absorb_env_pos_bond
	procedure:: absorb_env_name_bond
	procedure:: absorb_env_inner
	generic,public::absorb_env=>absorb_env_whole,absorb_env_pos_bond,absorb_env_name_bond
	procedure:: spit_env_whole
	procedure:: spit_env_pos_bond
	procedure:: spit_env_name_bond
	procedure:: spit_env_inner
	generic,public::spit_env=>spit_env_whole,spit_env_pos_bond,spit_env_name_bond
	!procedure:: generate_env_pos
	!procedure:: generate_env_name
	!generic,public::generate_env=>generate_env_pos,generate_env_name

	procedure:: copy_lat
	generic,public::assignment(=)=>copy_lat

	procedure,public:: transpose_lat
end type   

public lattice

contains

! I/O

subroutine write(L,funit)

	class(lattice),intent(inout) ::L
	integer,intent(in)::funit
	integer::i

	write(funit,*) L%empty_tag
	if(.not. L%empty_tag)then
		write(funit,*) L%L1
		write(funit,*) L%L2
		write(funit,*) L%max_nb_num
		write(funit,*) L%max_site_num
		call L%info%print(funit)
		call L%name_dic%print(funit)
		write(funit,*) L%raw_pos
		write(funit,*) L%name
		do i=1,L%max_site_num
			call write_site(L%sites(i),funit)
		end do
	end if

end subroutine

subroutine read(L,funit)

	class(lattice),intent(inout) ::L
	integer,intent(in)::funit
	integer::i

	call L%clean

	read(funit,*) L%empty_tag
	if(.not. L%empty_tag)then
		read(funit,*) L%L1
		read(funit,*) L%L2
		read(funit,*) L%max_nb_num
		read(funit,*) L%max_site_num
		allocate(L%sites(L%max_site_num))
		allocate(L%raw_pos(2*L%L1,2*L%L2))
		call L%info%read(funit)
		call L%name_dic%read(funit)
		read(funit,*) L%raw_pos
		read(funit,*) L%name
		do i=1,L%max_site_num
			call read_site(L%sites(i),funit)
		end do
	end if

end subroutine

subroutine write_site(S,funit)

	class(site),intent(inout) ::S
	integer,intent(in)::funit
	integer::i

	write(funit,*) S%exist_tag
	if(S%exist_tag)then
		write(funit,*) S%pos(2)
		write(funit,*) S%nb_num
		write(funit,*) trim(S%name)
		do i=1,S%nb_num
			write(funit,*) S%bonds(i)%nb_rawpos
			write(funit,*) S%bonds(i)%nb_no
			write(funit,*) S%bonds(i)%env_tag
			if(S%bonds(i)%env_tag)then
				call  S%bonds(i)%env%write(funit)
			end if 
			write(funit,*) trim(S%bonds(i)%ind)
		end do
		write(funit,*) associated(S%tensor)
		if(associated(S%tensor))then
			call S%tensor%write(funit)
		end if
		write(funit,*) S%con_tag
		call S%info%print(funit)
	end if

end subroutine


subroutine read_site(S,funit)

	class(site),intent(inout),target ::S
	integer,intent(in)::funit
	integer::i
	logical:: associated

	read(funit,*) S%exist_tag
	if(S%exist_tag)then
		read(funit,*) S%pos(2)
		read(funit,*) S%nb_num
		allocate(S%bonds(S%nb_num))
		read(funit,*) S%name
		do i=1,S%nb_num
			read(funit,*) S%bonds(i)%nb_rawpos
			read(funit,*) S%bonds(i)%nb_no
			read(funit,*) S%bonds(i)%env_tag
			if(S%bonds(i)%env_tag)then
				call S%bonds(i)%env%read(funit)
			end if 
			read(funit,*) S%bonds(i)%ind
		end do
		read(funit,*) associated
		if(associated)then
			S%tensor=>S%tensor_save
			call S%tensor%read(funit)
			S%tensor_save_tag=.true.
		end if
		read(funit,*) S%con_tag
		call S%info%read(funit)
	end if

end subroutine


! init

subroutine initialize(L,my_name,L1,L2,max_nb_num)

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

subroutine generate_ten(L,D,datatype)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::D
	character(len=*),intent(in)::datatype
	integer::i,k,nb_num,phy_dim

	call L%check_unempty()
	do i=1,L%max_site_num
		if(L%sites(i)%exist_tag) then
			nb_num=L%sites(i)%nb_num
			phy_dim=L%sites(i)%info%ii('Dp')
			L%sites(i)%tensor=>L%sites(i)%tensor_save
			L%sites(i)%tensor_save_tag=.true.
			if(phy_dim>0)then
				call L%sites(i)%tensor%allocate([(D,k=1,nb_num),phy_dim],datatype)
			else
				call L%sites(i)%tensor%allocate([(D,k=1,nb_num)],datatype)
			end if
			call L%sites(i)%tensor%random([-1.0d0,1.0d0])

			do k=1,nb_num
				call L%sites(i)%tensor%setName(k,L%sites(i)%bonds(k)%ind)
			end do
			if(phy_dim>0) call L%sites(i)%tensor%setName(nb_num+1,L%sites(i)%name+'.phy')
		end if
	end do

end subroutine

subroutine generate_env(L,D,datatype)

	class(lattice),intent(inout) ::L
	integer,intent(in)::D
	character(len=*),intent(in)::datatype
	integer::i,k,nb_num,phy_dim,nb_k,nb_rawpos

	call L%check_unempty()
	do i=1,L%max_site_num
		if(L%sites(i)%exist_tag) then
			nb_num=L%sites(i)%nb_num
			phy_dim=L%sites(i)%info%ii('Dp')

			do k=1,nb_num
				L%sites(i)%bonds(k)%env_tag=.true.
				nb_rawpos=L%sites(i)%bonds(k)%nb_rawpos
				if(nb_rawpos<i) then
					nb_k=L%sites(i)%bonds(k)%nb_no
					L%sites(i)%bonds(k)%env=L%sites(nb_rawpos)%bonds(nb_k)%env
				else
					call L%sites(i)%bonds(k)%env%allocate([D,D],datatype)	
					call L%sites(i)%bonds(k)%env%random([0d0,1.0d0])
					call L%sites(i)%bonds(k)%env%setName(1,'env.in')
					call L%sites(i)%bonds(k)%env%setName(2,'env.out')
				end if
			end do
		end if
	end do

end subroutine

! clean

subroutine clean(L)

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

	type(lattice),intent(inout) ::L

	call L%clean()

end subroutine

subroutine clean_site(S)

	type(site),intent(inout) ::S

	if(S%exist_tag)then
		S%pos=0
		deallocate(S%bonds)
		S%name=''
		call S%tensor_save%deallocate()
		S%tensor=>Null()
		S%nb_num=0
		S%con_tag=.false.
		S%exist_tag=.false.
		S%tensor_save_tag=.false.
	end if

end subroutine

! check legitimation

logical function check_exist_pos(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	
	call L%check_unempty()
	check_exist_pos=(L%raw_pos(pos(1),pos(2))>0)

end function 

logical function check_exist_name(L,name)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name
	
	call L%check_unempty()
	check_exist_name=(L%name_dic%val(name)>0)

end function 

logical function check_exist_bond_pos(L,pos1,pos2)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos1(2),pos2(2)
	logical::exist1,exist2
	integer::rawpos1,rawpos2

	call L%check_unempty()
	exist1=L%check_exist(pos1)
	exist2=L%check_exist(pos2)
	if(.not.(exist1 .and. exist2))then
		check_exist_bond_pos=.false.
		return
	end if
	rawpos1=L%get_rawpos(pos1)
	rawpos2=L%get_rawpos(pos2)

	check_exist_bond_pos=(L%bonds_num_rawpos(rawpos1,rawpos2)>0)

end function 

logical function check_exist_bond_name(L,name1,name2)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name1,name2
	logical::exist1,exist2
	integer::rawpos1,rawpos2

	call L%check_unempty()
	exist1=L%check_exist(name1)
	exist2=L%check_exist(name2)
	if(.not.(exist1 .and. exist2))then
		check_exist_bond_name=.false.
		return
	end if
	rawpos1=L%get_rawpos(name1)
	rawpos2=L%get_rawpos(name2)

	check_exist_bond_name=(L%bonds_num_rawpos(rawpos1,rawpos2)>0)
	
end function 

function bonds_num_pos(L,pos1,pos2) result(res)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos1(2),pos2(2)
	integer ::res
	integer::rawpos1,rawpos2

	call L%check_unempty()
	rawpos1=L%get_rawpos(pos1)
	rawpos2=L%get_rawpos(pos2)
	res=L%bonds_num_rawpos(rawpos1,rawpos2)

end function 

function bonds_num_name(L,name1,name2) result(res)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name1,name2
	integer ::res
	integer::rawpos1,rawpos2

	call L%check_unempty()
	rawpos1=L%get_rawpos(name1)
	rawpos2=L%get_rawpos(name2)
	res=L%bonds_num_rawpos(rawpos1,rawpos2)

end function 

function bonds_num_rawpos(L,rawpos1,rawpos2) result(res)

	class(lattice),intent(in) ::L
	integer,intent(in)::rawpos1,rawpos2
	integer ::res
	integer::i

	call L%check_unempty()
	res=0
	do i=1,L%sites(rawpos1)%nb_num
		if (L%sites(rawpos1)%bonds(i)%nb_rawpos==rawpos2) res=res+1
	end do

end function 

subroutine check_empty_whole(L)

	class(lattice),intent(in) ::L

	if(.not. L%empty_tag) then
		call writemess('Lattice is not empty!')
		call wc_error_stop
	end if
	
end subroutine

subroutine check_empty_site_pos(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)

	if(L%raw_pos(pos(1),pos(2))>0)then
		call writemess(trim(str(pos))//' is not empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if
	
end subroutine

subroutine check_empty_site_name(L,name)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name

	if(L%get_rawpos(name)>0)then
		call writemess(trim(name)//' is not empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if
	
end subroutine


subroutine check_unempty_whole(L)

	class(lattice),intent(in) ::L

	if(L%empty_tag) then
		call writemess('Lattice is empty!')
		call wc_error_stop
	end if
	
end subroutine

subroutine check_unempty_site_pos(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)

	if(L%raw_pos(pos(1),pos(2))==0)then
		call writemess(trim(str(pos))//' is empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if
	
end subroutine

subroutine check_unempty_site_name(L,name)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name

	if(L%get_rawpos(name)==0)then
		call writemess(trim(name)//' is empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if
	
end subroutine

subroutine check_boundary(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)

	call L%check_unempty()
	if(pos(1)<0.or.pos(1)>L%L1.or.pos(2)<0.or.pos(2)>L%L2) then
		call writemess('Site ('+pos(1)+','+pos(2)+') is out of boundary of a_'+L%L1+'X'+L%L2+'_lattice:'+L%name)
		call wc_error_stop
	end if
	
end subroutine

subroutine check_consistency(L)

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
		pass2=pass2.and.(L%name_dic%num()==0)
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
		num3=L%name_dic%num()
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
					nb_rawpos=L%sites(i)%bonds(k)%nb_rawpos
					nb_no=L%sites(i)%bonds(k)%nb_no
					if(nb_rawpos==i)then
						pass(4)=.false.
						call writemess('site ('+i+'-'+k+') links to itself')
					end if
					if(nb_rawpos>i)then
						nb_nb_rawpos=L%sites(nb_rawpos)%bonds(nb_no)%nb_rawpos
						nb_nb_no=L%sites(nb_rawpos)%bonds(nb_no)%nb_no
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

	class(site),intent(in)::S
	integer::i

	site_consistent=.true.
	if(.not. S%exist_tag) then
		site_consistent=site_consistent.and.(len_trim(S%name)==0)
		site_consistent=site_consistent.and.(.not.allocated(S%bonds))
		site_consistent=site_consistent.and.all(S%pos==0)
		site_consistent=site_consistent.and.(.not.S%con_tag)
		!site_consistent=site_consistent.and.(.not.associated(S%tensor))
		site_consistent=site_consistent.and.(S%nb_num==0)
		if(.not. site_consistent)then
			write(*,*)len_trim(S%name)==0
			write(*,*)all(S%pos==0)
			write(*,*).not.S%con_tag
			!write(*,*).not.associated(S%tensor)
			write(*,*)S%nb_num==0
			write(*,*).not.allocated(S%bonds)
		end if
		return
	else
		site_consistent=site_consistent.and.(len_trim(S%name)>0)
		site_consistent=site_consistent.and.(size(S%bonds)>=S%nb_num)
		if(associated(S%tensor)) then
			do i=1,S%nb_num
				site_consistent=site_consistent.and.(S%tensor%nameorder(S%bonds(i)%ind)>0)
			end do
		end if
		if(.not. site_consistent)then
			write(*,*)'position is: ',S%pos
			write(*,*)'name is',trim(S%name)
			write(*,*)'max neighbor num is: ',size(S%bonds)
			write(*,*)'neighbor num is: ',S%nb_num
			if(S%nb_num>0) then
				write(*,*)'indices are '
				do i=1,S%nb_num
					write(*,*) trim(S%bonds(i)%ind)
				end do
			end if
			write(*,*)'points to a tensor ? ',associated(S%tensor)
			if(associated(S%tensor)) call S%tensor%diminfo()
		end if
		return
	end if

end function

! env

subroutine absorb_env_whole(L)

	class(lattice),intent(inout)::L
	integer::i,k

	call L%check_unempty()
	do i=1,L%max_site_num
		if(L%sites(i)%exist_tag)then
			if(L%sites(i)%info%li('absorb_env'))then
				do k=1,L%sites(i)%nb_num
					call L%absorb_env_inner(i,k)
				end do
			end if
		end if
	end do

end subroutine

subroutine absorb_env_pos_bond(L,pos,no)

	class(lattice),intent(inout)::L
	integer,intent(in)::pos(2),no
	integer::n,rawpos
	type(tensor)::temp

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)

	if(L%sites(rawpos)%info%li('absorb_env'))then
		call L%absorb_env_inner(rawpos,no)
	end if

end subroutine

subroutine absorb_env_name_bond(L,name,no)

	class(lattice),intent(inout)::L
	character(len=*),intent(in)::name
	integer,intent(in)::no
	integer::n,rawpos
	type(tensor)::temp

	call L%check_unempty()
	rawpos=L%get_rawpos(name)

	if(L%sites(rawpos)%info%li('absorb_env'))then
		call L%absorb_env_inner(rawpos,no)
	end if

end subroutine


subroutine absorb_env_inner(L,rawpos,no)

	class(lattice),intent(inout)::L
	integer,intent(in)::rawpos,no
	integer::n
	type(tensor)::temp

	temp=L%sites(rawpos)%bonds(no)%env
	do n=1,temp%dim(1)
		call temp%setValue([n,n], dsqrt(temp%di([n,n])) )
	end do
	L%sites(rawpos)%tensor=contract(L%sites(rawpos)%tensor,L%sites(rawpos)%bonds(no)%ind,temp,1)
	call L%sites(rawpos)%tensor%setName(L%sites(rawpos)%tensor%getRank(),L%sites(rawpos)%bonds(no)%ind)

end subroutine

subroutine spit_env_whole(L)

	class(lattice),intent(inout)::L
	integer::i,k

	call L%check_unempty()
	do i=1,L%max_site_num
		if(L%sites(i)%exist_tag)then
			if(L%sites(i)%info%li('absorb_env'))then
				do k=1,L%sites(i)%nb_num
					call L%spit_env_inner(i,k)
				end do
			end if
		end if
	end do

end subroutine

subroutine spit_env_pos_bond(L,pos,no)

	class(lattice),intent(inout)::L
	integer,intent(in)::pos(2),no
	integer::n,rawpos
	type(tensor)::temp

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)

	if(L%sites(rawpos)%info%li('absorb_env'))then
		call L%spit_env_inner(rawpos,no)
	end if

end subroutine

subroutine spit_env_name_bond(L,name,no)

	class(lattice),intent(inout)::L
	character(len=*),intent(in)::name
	integer,intent(in)::no
	integer::n,rawpos
	type(tensor)::temp

	call L%check_unempty()
	rawpos=L%get_rawpos(name)

	if(L%sites(rawpos)%info%li('absorb_env'))then
		call L%spit_env_inner(rawpos,no)
	end if

end subroutine

subroutine spit_env_inner(L,rawpos,no)

	class(lattice),intent(inout)::L
	integer,intent(in)::rawpos,no
	integer::n
	type(tensor)::temp

	temp=L%sites(rawpos)%bonds(no)%env
	do n=1,temp%dim(1)
		call temp%setValue([n,n], 1/dsqrt(temp%di([n,n])) )
	end do
	L%sites(rawpos)%tensor=contract(L%sites(rawpos)%tensor,L%sites(rawpos)%bonds(no)%ind,temp,1)
	call L%sites(rawpos)%tensor%setName(L%sites(rawpos)%tensor%getRank(),L%sites(rawpos)%bonds(no)%ind)

end subroutine

! copy

subroutine copy_lat(L,L_old)

	class(lattice),intent(inout)::L
	type(lattice),intent(in)::L_old
	character(len=max_char_length)::dir,dir2,name,name2
	logical::exist,exist2
	integer::i,j,nb_rawpos,nb_no

	call L%clean()
	if(.not. L_old%empty_tag)then
		call L%initialize(L_old%name,L_old%L1,L_old%L2,L_old%max_nb_num)
		L%name_dic=L_old%name_dic
		L%info=L_old%info
		L%raw_pos=L_old%raw_pos
		L%max_site_num=L_old%max_site_num
		do i=1,L_old%max_site_num
			if(L_old%sites(i)%exist_tag)then
				L%sites(i)=L_old%sites(i)
			end if
		end do
	end if

end subroutine

subroutine set_bond_as(L,L_old)

	class(lattice),intent(inout)::L
	type(lattice),intent(in)::L_old
	character(len=max_char_length)::dir,dir2,name,name2
	integer::i,j,nb_rawpos,nb_no

	call L%check_unempty()
	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			do j=1,L_old%sites(i)%nb_num
				nb_rawpos=L_old%sites(i)%bonds(j)%nb_rawpos
				if(nb_rawpos>i) then
					name=L_old%sites(i)%name
					name2=L_old%sites(nb_rawpos)%name
					if(L%check_exist(name).and.L%check_exist(name2) .and. .not. L%check_exist_bond(name,name2))then
						nb_no=L_old%sites(i)%bonds(j)%nb_no
						dir=L_old%sites(i)%bonds(j)%ind
						dir2=L_old%sites(nb_rawpos)%bonds(nb_no)%ind
						call L%set_bond(name,name2,dir,dir2)
					end if
				end if
			end do
		end if
	end do
			
end subroutine


function transpose_lat(L_old) result(L)

	type(lattice)::L
	class(lattice),intent(in)::L_old
	integer::i,j,pos(2),nb_rawpos,nb_no
	character(len=max_char_length)::dir,dir2,name,name2
	type(dictionary),pointer::pinfo

	if(.not. L_old%empty_tag)then
		call L%initialize(L_old%name,L_old%L2,L_old%L1,L_old%max_nb_num)
		L%info=L_old%info

		do i=1,L_old%max_site_num
			if(L_old%sites(i)%exist_tag)then
				pos=L_old%sites(i)%pos
				call L%add([pos(2),pos(1)],L_old%sites(i)%name,L_old%sites(i)%tensor,L_old%sites(i)%tensor_save_tag)
				if(L_old%sites(i)%con_tag) call L%set_contag(L_old%sites(i)%name,.true.)
				call L%point_info(L_old%sites(i)%name,pinfo)
				pinfo=L_old%sites(i)%info
			end if
		end do

		do i=1,L_old%max_site_num
			if(L_old%sites(i)%exist_tag)then
				do j=1,L_old%sites(i)%nb_num
					nb_rawpos=L_old%sites(i)%bonds(j)%nb_rawpos
					if(nb_rawpos>=i)then
						nb_no=L_old%sites(i)%bonds(j)%nb_no
						dir=L_old%sites(i)%bonds(j)%ind
						dir2=L_old%sites(nb_rawpos)%bonds(nb_no)%ind
						name=L_old%sites(i)%name
						name2=L_old%sites(nb_rawpos)%name
						call L%set_bond(name,name2,dir,dir2)
					end if
				end do
			end if
		end do
	end if
			
end function

subroutine copy_line(L,line,L_old,line2,nline)

	class(lattice),intent(inout)::L
	type(lattice),intent(in)::L_old
	integer,intent(in)::line,line2,nline
	integer::i,j,k,L1_old,L2_old,pos(2),nb_pos(2),nb_rawpos,nb_no
	character(len=max_char_length)::dir,dir2,name,name2

	call L%check_unempty()
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
				call L%add([pos(1)-line2+line,pos(2)],L_old%sites(i)%name,L_old%sites(i)%tensor,L_old%sites(i)%tensor_save_tag)
				if(L_old%sites(i)%con_tag) call L%set_contag(L_old%sites(i)%name,.true.)
			end if
		end if
	end do

	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			pos=L_old%sites(i)%pos
			if(pos(1)>=line2 .and. pos(1)<=line2+nline-1) then
				do j=1,L_old%sites(i)%nb_num
					nb_rawpos=L_old%sites(i)%bonds(j)%nb_rawpos
					nb_pos=L_old%sites(nb_rawpos)%pos
					if(nb_rawpos>i .and. nb_pos(1)>=line2 .and. nb_pos(1)<=line2+nline-1) then
						nb_no=L_old%sites(i)%bonds(j)%nb_no
						dir=L_old%sites(i)%bonds(j)%ind
						dir2=L_old%sites(nb_rawpos)%bonds(nb_no)%ind
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

	class(lattice),intent(inout),target::L
	type(lattice),intent(in)::L_old
	integer,intent(in)::line2,nline
	integer::i,j,k,L1_old,L2_old,pos(2),nb_pos(2),rawpos,nb_rawpos,nb_no,tn_num
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
				call L%add([pos(1)-line2+1,pos(2)],L_old%sites(i)%name,L_old%sites(i)%tensor,.false.)
				call L%set_contag(L_old%sites(i)%name,L_old%sites(i)%con_tag)
				call L%add([line2+2*nline-pos(1),pos(2)],'mir_'+L_old%sites(i)%name,L_old%sites(i)%tensor,.true.)
				call L%set_contag('mir_'+L_old%sites(i)%name,.not. L_old%sites(i)%con_tag)
			end if
		end if
	end do

	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			pos=L_old%sites(i)%pos
			if(pos(1)>=line2 .and. pos(1)<=line2+nline-1) then
				do k=1,L_old%sites(i)%nb_num
					nb_rawpos=L_old%sites(i)%bonds(k)%nb_rawpos
					nb_pos=L_old%sites(nb_rawpos)%pos
					name=L_old%sites(i)%name
					name2=L_old%sites(nb_rawpos)%name
					nb_no=L_old%sites(i)%bonds(k)%nb_no
					ind=L_old%sites(i)%bonds(k)%ind
					ind2=L_old%sites(nb_rawpos)%bonds(nb_no)%ind
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

! basic operations

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

subroutine add(L,pos,my_name_,my_tensor,save_tag_)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	character(len=*), intent(in),optional :: my_name_
	type(tensor),intent(in),optional,target::my_tensor
	logical,intent(in),optional::save_tag_
	character(len=max_char_length)::my_name
	logical :: save_tag
	integer::rawpos

	call L%check_unempty()
	if(present(my_name_))then
		my_name=my_name_
	else
		if(present(my_tensor))then
			my_name=my_tensor.Tname.1
		else
			call wc_error_stop('lattice.add','input should contains name')
		end if
	end if

	if(present(save_tag_))then
		save_tag=save_tag_
	else
		save_tag=.false.
	end if

	call L%check_empty(pos)
	call L%name_dic%add(my_name,rawpos)
	L%raw_pos(pos(1),pos(2))=rawpos
	L%sites(rawpos)%pos=pos
	L%sites(rawpos)%name=my_name
	L%sites(rawpos)%exist_tag=.true.
	allocate(L%sites(rawpos)%bonds(L%max_nb_num))
	if(present(my_tensor)) call L%set_tensor(my_name,my_tensor,save_tag)

end subroutine

subroutine rename_pos(L,pos,new_name)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	character(len=*), intent(in) :: new_name
	integer::rawpos

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	call L%name_dic%rename(L%sites(rawpos)%name,new_name)
	L%sites(rawpos)%name=new_name

end subroutine

subroutine rename_name(L,old_name,new_name)

	class(lattice),intent(inout) ::L
	character(len=*), intent(in) :: old_name,new_name
	integer::rawpos

	call L%check_unempty()
	rawpos=L%get_rawpos(old_name)
	call L%name_dic%rename(old_name,new_name)
	L%sites(rawpos)%name=new_name

end subroutine

subroutine point_info_lat(L,dic)

	class(lattice),intent(inout),target ::L
	type(dictionary),pointer, intent(inout) :: dic

	call L%check_unempty()
	dic=>L%info

end subroutine

subroutine point_info_pos(L,pos,dic)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	type(dictionary),pointer, intent(inout) :: dic
	integer::rawpos

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	dic=>L%sites(rawpos)%info

end subroutine

subroutine point_info_name(L,name,dic)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	type(dictionary),pointer, intent(inout) :: dic
	integer::rawpos

	call L%check_unempty()
	rawpos=L%get_rawpos(name)
	dic=>L%sites(rawpos)%info

end subroutine

subroutine delete_pos(L,pos)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	integer::rawpos,nb_rawpos,i

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	call L%name_dic%del(L%sites(rawpos)%name)
	L%raw_pos(pos(1),pos(2))=0
	do i=1,L%sites(rawpos)%nb_num
		nb_rawpos=L%sites(rawpos)%bonds(i)%nb_rawpos
		call L%remove_nb(nb_rawpos,rawpos)
	end do
	call clean_site(L%sites(rawpos))

end subroutine

subroutine delete_name(L,name)

	class(lattice),intent(inout) ::L
	character(len=*), intent(in) :: name
	integer::rawpos,pos(2),nb_rawpos,i

	call L%check_unempty()
	rawpos=L%get_rawpos(name)
	call L%name_dic%del(name)
	pos=L%sites(rawpos)%pos
	L%raw_pos(pos(1),pos(2))=0
	do i=1,L%sites(rawpos)%nb_num
		nb_rawpos=L%sites(rawpos)%bonds(i)%nb_rawpos
		call L%remove_nb(nb_rawpos,rawpos)
	end do
	call clean_site(L%sites(rawpos))

end subroutine

subroutine move_pos(L,pos,new_pos)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2),new_pos(2)
	integer::rawpos

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	call L%check_empty(new_pos)
	L%raw_pos(pos(1),pos(2))=0
	L%raw_pos(new_pos(1),new_pos(2))=rawpos
	L%sites(rawpos)%pos=new_pos

end subroutine

subroutine move_name(L,name,new_pos)

	class(lattice),intent(inout) ::L
	character(len=*),intent(in) :: name
	integer,intent(in)::new_pos(2)
	integer::rawpos,old_pos(2)

	call L%check_unempty()
	rawpos=L%get_rawpos(name)
	call L%check_empty(new_pos)
	old_pos=L%sites(rawpos)%pos
	L%raw_pos(old_pos(1),old_pos(2))=0
	L%raw_pos(new_pos(1),new_pos(2))=rawpos
	L%sites(rawpos)%pos=new_pos

end subroutine

type(tensor) function get_tensor_pos(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	integer::rawpos
	
	call L%check_unempty()	
	rawpos=L%get_rawpos(pos)
	get_tensor_pos=L%sites(rawpos)%tensor

end function

type(tensor) function get_tensor_name(L,name)

	class(lattice),intent(in) ::L
	character(len=*),intent(in) :: name
	integer::rawpos
	
	call L%check_unempty()	
	rawpos=L%get_rawpos(name)
	get_tensor_name=L%sites(rawpos)%tensor

end function

subroutine get_tensor_link_pos(L,pos,tlink)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	type(tensor),pointer,intent(out)::tlink
	integer::rawpos
	
	call L%check_unempty()	
	rawpos=L%get_rawpos(pos)
	tlink=>L%sites(rawpos)%tensor

end subroutine

subroutine get_tensor_link_name(L,name,tlink)

	class(lattice),intent(in) ::L
	character(len=*),optional,intent(in)::name
	type(tensor),pointer,intent(out)::tlink
	integer::rawpos
	
	call L%check_unempty()	
	rawpos=L%get_rawpos(name)
	tlink=>L%sites(rawpos)%tensor

end subroutine

subroutine get_env_link_pos(L,pos,no,tlink)

	class(lattice),intent(in),target ::L
	integer,intent(in)::pos(2),no
	type(tensor),pointer,intent(out)::tlink
	integer::rawpos
	
	call L%check_unempty()	
	rawpos=L%get_rawpos(pos)
	if(.not.L%sites(rawpos)%bonds(no)%env_tag)then
		call wc_error_stop('lattice.get_env_link','env at '//trim(str(pos))//'-'//trim(str(no))//' does not exist')
	end if
	tlink=>L%sites(rawpos)%bonds(no)%env

end subroutine

subroutine get_env_link_name(L,name,no,tlink)

	class(lattice),intent(in),target ::L
	character(len=*),optional,intent(in)::name
	integer,intent(in)::no
	type(tensor),pointer,intent(out)::tlink
	integer::rawpos
	
	call L%check_unempty()	
	rawpos=L%get_rawpos(name)
	if(.not.L%sites(rawpos)%bonds(no)%env_tag)then
		call wc_error_stop('lattice.get_env_link','env at '//trim(name)//'-'//trim(str(no))//' does not exist')
	end if
	tlink=>L%sites(rawpos)%bonds(no)%env

end subroutine

subroutine update_tensor_pos(L,pos,new_tensor)

	class(lattice),intent(inout) ::L
	type(tensor),intent(in)::new_tensor
	integer,intent(in)::pos(2)
	integer::rawpos

	call L%check_unempty()		
	rawpos=L%get_rawpos(pos)
	L%sites(rawpos)%tensor=new_tensor

end subroutine

subroutine update_tensor_name(L,name,new_tensor)

	class(lattice),intent(inout) ::L
	type(tensor),intent(in)::new_tensor
	character(len=*),intent(in)::name
	integer::rawpos
		
	call L%check_unempty()
	rawpos=L%get_rawpos(name)
	L%sites(rawpos)%tensor=new_tensor

end subroutine

subroutine set_tensor_name(L,name,my_tensor,save_tag_)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	type(tensor),intent(in),target::my_tensor
	logical,intent(in),optional::save_tag_
	logical :: save_tag
	integer::rawpos

	if(present(save_tag_))then
		save_tag=save_tag_
	else
		save_tag=.false.
	end if
		
	call L%check_unempty()
	rawpos=L%get_rawpos(name)
	if(save_tag)then
		L%sites(rawpos)%tensor_save=my_tensor
		L%sites(rawpos)%tensor=>L%sites(rawpos)%tensor_save
	else
		L%sites(rawpos)%tensor=>my_tensor
	end if 
	L%sites(rawpos)%tensor_save_tag=save_tag

end subroutine

subroutine set_tensor_pos(L,pos,my_tensor,save_tag_)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	type(tensor),intent(in),target::my_tensor
	logical,intent(in),optional::save_tag_
	logical :: save_tag
	integer::rawpos

	if(present(save_tag_))then
		save_tag=save_tag_
	else
		save_tag=.false.
	end if

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	if(save_tag)then
		L%sites(rawpos)%tensor_save=my_tensor
		L%sites(rawpos)%tensor=>L%sites(rawpos)%tensor_save
	else
		L%sites(rawpos)%tensor=>my_tensor
	end if 
	L%sites(rawpos)%tensor_save_tag=save_tag

end subroutine

subroutine set_bond_pos(L,pos,pos2,dir_,dir2_)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2),pos2(2)
	character(len=*),optional,intent(in)::dir_,dir2_
	character(len=max_char_length)::dir,dir2
	logical::exist,exist2
	integer::rawpos,rawpos2
	
	call L%check_unempty()
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

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::name,name2
	character(len=*),optional,intent(in)::dir_,dir2_
	character(len=max_char_length)::dir,dir2
	logical::exist,exist2
	integer::rawpos,rawpos2
	
	call L%check_unempty()
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

	if(exist) call wc_error_stop('set_bond',trim(dir)//' existed at '//trim(name)//' for lattice '//trim(L%name))

	if(exist2) call wc_error_stop('set_bond',trim(dir2)//' existed at '//trim(name2)//' for lattice '//trim(L%name))

end subroutine

subroutine set_bond_rawpos(L,rawpos,dir,rawpos2,dir2,exist,exist2)

	class(lattice),intent(inout) ::L
	integer,intent(in)::rawpos,rawpos2
	character(len=*),intent(in)::dir,dir2
	logical,intent(out)::exist,exist2
	integer::k,no,nb_no,nb_pos(2)

	exist=.false.
	exist2=.false.

	call L%check_unempty()
	do k=1,L%sites(rawpos)%nb_num
		if(L%sites(rawpos)%bonds(k)%ind==dir)then
			exist=.true.
			return
		end if
	end do

	do k=1,L%sites(rawpos2)%nb_num
		if(L%sites(rawpos2)%bonds(k)%ind==dir2)then
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

	L%sites(rawpos)%bonds(no)%nb_rawpos=rawpos2
	L%sites(rawpos)%bonds(no)%ind=dir
	L%sites(rawpos)%bonds(no)%nb_no=nb_no

	L%sites(rawpos2)%bonds(nb_no)%nb_rawpos=rawpos
	L%sites(rawpos2)%bonds(nb_no)%ind=dir2
	L%sites(rawpos2)%bonds(nb_no)%nb_no=no

end subroutine

character(len=max_char_length) function ind_name(L,pos,no) result(ind)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2),no
	integer::rawpos
	
	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	if(no>L%sites(rawpos)%nb_num) then
		call wc_error_stop('ind_name','Bond NO.'+no+' at ('+pos(1)+','+pos(2)+') does not exist.')
	end if
	ind=L%sites(rawpos)%bonds(no)%ind

end function

subroutine get_size(L,L1,L2)

	class(lattice),intent(in) ::L
	integer,intent(out)::L1,L2
	integer::k

	call L%check_unempty()
	L1=L%L1
	L2=L%L2

end subroutine


integer function get_max_site_num(L)

	class(lattice),intent(in) ::L

	call L%check_unempty()
	get_max_site_num =  L%max_site_num

end function

subroutine get_bond_pos(L,pos,no,ind,nb_pos,nb_no,nb_ind)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2),nb_pos(2)
	integer,intent(out)::no,nb_no
	character(len=max_char_length),intent(out)::ind,nb_ind
	integer::k,rawpos,nb_rawpos
	
	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	nb_rawpos=L%get_rawpos(nb_pos)
	do k=1,L%sites(rawpos)%nb_num
		if (L%sites(rawpos)%bonds(k)%nb_rawpos==nb_rawpos) then
			no=k
			nb_no=L%sites(rawpos)%bonds(k)%nb_no
			ind=L%sites(rawpos)%bonds(k)%ind
			nb_ind=L%sites(nb_rawpos)%bonds(nb_no)%ind
			return
		end if
	end do 

	call wc_error_stop('get_bond_pos','Bond from ('+pos(1)+','+pos(2)+') to ('+nb_pos(1)+','+nb_pos(2)+') does not exist.')

end subroutine

subroutine get_bond_name(L,name,no,ind,nb_name,nb_no,nb_ind)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name,nb_name
	integer,intent(out)::no,nb_no
	character(len=max_char_length),intent(out)::ind,nb_ind
	integer::k,rawpos,nb_rawpos
	
	call L%check_unempty()
	rawpos=L%get_rawpos(name)
	nb_rawpos=L%get_rawpos(nb_name)
	do k=1,L%sites(rawpos)%nb_num
		if (L%sites(rawpos)%bonds(k)%nb_rawpos==nb_rawpos) then
			no=k
			nb_no=L%sites(rawpos)%bonds(k)%nb_no
			ind=L%sites(rawpos)%bonds(k)%ind
			nb_ind=L%sites(nb_rawpos)%bonds(nb_no)%ind
			return
		end if
	end do 

	call wc_error_stop('get_bond_pos','Bond from '+name+' to '+nb_name+' does not exist.')

end subroutine

subroutine get_bond_no_pos(L,pos,no,ind,nb_pos,nb_no,nb_ind)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2),no
	integer,intent(out)::nb_no,nb_pos(2)
	character(len=max_char_length),intent(out)::ind,nb_ind
	integer::k,rawpos,nb_rawpos
	
	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	if(no>L%sites(rawpos)%nb_num) then
		call wc_error_stop('get_bond_no','Bond NO.'+no+' at ('+pos(1)+','+pos(2)+') does not exist.')
	end if

	nb_rawpos=L%sites(rawpos)%bonds(no)%nb_rawpos
	nb_no=L%sites(rawpos)%bonds(no)%nb_no
	ind=L%sites(rawpos)%bonds(no)%ind
	nb_pos=L%sites(nb_rawpos)%pos
	nb_ind=L%sites(nb_rawpos)%bonds(nb_no)%ind

end subroutine

subroutine get_bond_no_name(L,name,no,ind,nb_name,nb_no,nb_ind)

	class(lattice),intent(in) ::L
	integer,intent(in)::no
	character(len=*),intent(in)::name
	integer,intent(out)::nb_no
	character(len=max_char_length),intent(out)::nb_name
	character(len=max_char_length),intent(out)::ind,nb_ind
	integer::k,rawpos,nb_rawpos
	
	call L%check_unempty()
	rawpos=L%get_rawpos(name)
	if(no>L%sites(rawpos)%nb_num) then
		call wc_error_stop('get_bond_no','Bond NO.'+no+' at ('+name+') does not exist.')
	end if

	nb_rawpos=L%sites(rawpos)%bonds(no)%nb_rawpos
	nb_no=L%sites(rawpos)%bonds(no)%nb_no
	ind=L%sites(rawpos)%bonds(no)%ind
	nb_name=L%sites(nb_rawpos)%name
	nb_ind=L%sites(nb_rawpos)%bonds(nb_no)%ind

end subroutine

character(len=max_char_length) function get_name_whole(L)

	class(lattice),intent(in) ::L

	call L%check_unempty()
	get_name_whole=L%name

end function

character(len=max_char_length) function get_name_site(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	integer::rawpos


	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	get_name_site=L%sites(rawpos)%name

end function

subroutine set_name(L,my_name)

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::my_name

	call L%check_unempty()
	L%name=my_name
	
end subroutine

integer function get_nb_num(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	integer::rawpos

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	get_nb_num=L%sites(rawpos)%nb_num

end function

logical function get_contag(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	integer::rawpos

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	get_contag=L%sites(rawpos)%con_tag

end function

subroutine set_contag_pos(L,pos,status)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	logical,intent(in)::status
	integer::rawpos

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	L%sites(rawpos)%con_tag=status

end subroutine

subroutine set_contag_name(L,name,status)

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::name
	logical,intent(in)::status
	integer::rawpos

	call L%check_unempty()
	rawpos=L%get_rawpos(name)
	L%sites(rawpos)%con_tag=status

end subroutine

subroutine invert_bond(L,pos,temp)

	class(lattice),intent(in)::L
	type(tensor),intent(inout)::temp
	integer,intent(in)::pos(2)
	integer::no,nb_pos(2),nb_no,rawpos,nb_rawpos

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	do no=1,L%sites(rawpos)%nb_num
		nb_rawpos=L%sites(rawpos)%bonds(no)%nb_rawpos
		nb_no=L%sites(rawpos)%bonds(no)%nb_no
		call temp%setName(L%sites(nb_rawpos)%bonds(nb_no)%ind,L%sites(rawpos)%bonds(no)%ind)
	end do

end subroutine

integer function cut_bonds(L,i) !bond between ith and i+1th line

	class(lattice),intent(in)::L
	integer,intent(in)::i
	integer::j,k,no,L1,L2,rawpos,nb_rawpos,nb_pos(2)

	call L%check_unempty()
	call check_boundary(L,[i,1])
	cut_bonds=0
	!do j=1,i
		do k=1,L%L2
			rawpos=L%raw_pos(i,k)
			if(rawpos>0)then
				do no=1,L%sites(rawpos)%nb_num
					nb_rawpos=L%sites(rawpos)%bonds(no)%nb_rawpos
					if(L%sites(nb_rawpos)%pos(1)==i+1)then	! issue: >i or ==i+1, essential to pbc and cyl
						cut_bonds=cut_bonds+1
					end if
				end do
			end if
		end do		
	!end do	

end function 

integer function max_cut_bonds(L)

	class(lattice),intent(in)::L
	integer::i

	call L%check_unempty()
	max_cut_bonds=0
	do i=1,L%L1-1
		max_cut_bonds=max(max_cut_bonds,L%cut_bonds(i))
	end do

end function 

!!!!!!!!!!!!!!!!!!!!!!!!! Dynamic lattice !!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine remove_nb(L,rawpos,nb_rawpos)
! at rawpos, remove nb_rawpos

	class(lattice),intent(inout) ::L
	integer,intent(in)::rawpos,nb_rawpos
	integer::i,num,nb_rawpos2,nb_no2
	integer,allocatable::move(:)

	call L%check_unempty()
	allocate(move(L%sites(rawpos)%nb_num))

	num=0
	do i=1,L%sites(rawpos)%nb_num
		nb_rawpos2=L%sites(rawpos)%bonds(i)%nb_rawpos
		if(nb_rawpos2/=nb_rawpos) then
			num=num+1
			if(num/=i) then
				nb_no2=L%sites(rawpos)%bonds(i)%nb_no
				L%sites(nb_rawpos2)%bonds(nb_no2)%nb_no=num
				L%sites(rawpos)%bonds(num)=L%sites(rawpos)%bonds(i)
			end if
		end if
	end do
	L%sites(rawpos)%nb_num=num
	
end subroutine

subroutine move_nb(L,rawpos,nb_rawpos1,nb_rawpos2)
! at rawpos, move bonds connected to nb_rawpos1 to nb_rawpos2

	class(lattice),intent(inout),target ::L
	integer,intent(in)::rawpos,nb_rawpos1,nb_rawpos2
	integer::i,nb_no,bond_num,nb_no2
	character(len=max_char_length)::old_name,old_name2,new_name,new_name2
	type(bond),pointer::this_bond,nb_bond,nb_bond2

	call L%check_unempty()
	do i=1,L%sites(rawpos)%nb_num
		if(L%sites(rawpos)%bonds(i)%nb_rawpos==nb_rawpos1) then
			L%sites(nb_rawpos2)%nb_num=L%sites(nb_rawpos2)%nb_num+1
			nb_no2=L%sites(nb_rawpos2)%nb_num
			nb_no=L%sites(rawpos)%bonds(i)%nb_no

			this_bond=>L%sites(rawpos)%bonds(i)
			nb_bond=>L%sites(nb_rawpos1)%bonds(nb_no)
			nb_bond2=>L%sites(nb_rawpos2)%bonds(nb_no2)

			nb_bond2=nb_bond
			this_bond%nb_rawpos=nb_rawpos2
			this_bond%nb_no=nb_no2

			bond_num=L%bonds_num_rawpos(rawpos,nb_rawpos2)

			old_name=this_bond%ind
			new_name=L%sites(rawpos)%name+'.'+L%sites(nb_rawpos2)%name
			if(bond_num>1) new_name=new_name+(bond_num+1)
			call L%sites(rawpos)%tensor%setName(old_name,new_name)
			this_bond%ind=new_name

			old_name2=nb_bond2%ind
			new_name2=L%sites(nb_rawpos2)%name+'.'+L%sites(rawpos)%name
			if(bond_num>1) new_name2=new_name2+(bond_num+1)
			call L%sites(nb_rawpos2)%tensor%setName(old_name2,new_name2)
			nb_bond2%ind=new_name2
		end if
	end do
	
end subroutine

subroutine absorb(L,pos,nb_pos)

	class(lattice),intent(inout)::L
	integer,intent(in)::pos(2),nb_pos(2)
	character(len=max_char_length)::leg(8),leg_nb(8)
	integer::i,num,nb(2),nb_no,rawpos,nb_rawpos
	integer,allocatable::move(:)

	call L%check_unempty()
	rawpos=L%get_rawpos(pos)
	nb_rawpos=L%get_rawpos(nb_pos)

	num=0
	do i=1,L%sites(rawpos)%nb_num
		if(L%sites(rawpos)%bonds(i)%nb_rawpos==nb_rawpos) then
			num=num+1
			nb_no=L%sites(rawpos)%bonds(i)%nb_no
			leg(num)=L%sites(rawpos)%bonds(i)%ind
			leg_nb(num)=L%sites(nb_rawpos)%bonds(nb_no)%ind
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
		if(L%sites(nb_rawpos)%bonds(i)%nb_rawpos/=rawpos)then
			call L%move_nb(L%sites(nb_rawpos)%bonds(i)%nb_rawpos,nb_rawpos,rawpos)
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

subroutine draw_l(L,filename,mode)

	class(lattice),intent(in)::L
	character(len=*),intent(in)::filename
	character(len=2),intent(in)::mode
	integer::i

	call L%draw_tn(filename,L%name,[(.false.,i=1,L%max_site_num)],mode)

end subroutine


subroutine draw_tn(L,filename,tnname,includes,mode)

	class(lattice),intent(in)::L
	character(len=*),intent(in)::filename,tnname
	character(len=2),intent(in)::mode
	logical,intent(in)::includes(:)
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
				nb=L%sites(i)%bonds(k)%nb_rawpos
				nb_no=L%sites(i)%bonds(k)%nb_no
				if(nb>i)then
					if(mode(1:1)=='e' )then
						command='  node_'+i+' -- node_'+nb
						if(includes(i).and.includes(nb)) then
							command=command+' [color="red"]'
						end if
						write(7878,'(A)')trim(command)
					else if(mode(1:1)=='n')then
						command='  node_'+i+' -- node_'+nb+' [taillabel = "'+after_dot(L%sites(i)%bonds(k)%ind) &
							+'",headlabel ="'+after_dot(L%sites(nb)%bonds(nb_no)%ind)+'"'
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
	call L%check_consistency()
	read(*,*)

end subroutine


!!!!!!!!!!!!!!!!!!!!!!! Used in tn_tensor !!!!!!!!!!!!!!!!!!!!!!

subroutine absorb_tensor(L,fn_tensor,ori_tensor,pos,includes,mask)

	class(lattice),intent(inout)::L
	type(tensor),intent(inout)::ori_tensor
	type(tensor),intent(inout)::fn_tensor
	logical,intent(inout)::includes(:)
	logical,intent(in),optional::mask(:)
	integer,intent(in)::pos(2)
	character(len=max_char_length)::leg(8),leg_nb(8)
	integer::k,num,nb(2),nb_no,rawpos,nb_rawpos
	type(tensor)::testen
	complex(8),pointer::tendata(:)

	call L%check_unempty()
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
			nb_rawpos=L%sites(rawpos)%bonds(k)%nb_rawpos
			nb_no=L%sites(rawpos)%bonds(k)%nb_no
			if(includes(nb_rawpos)) then
				num=num+1
				leg(num)=L%sites(rawpos)%bonds(k)%ind
				leg_nb(num)=L%sites(nb_rawpos)%bonds(nb_no)%ind
			end if
		end do

		if(L%sites(rawpos)%con_tag) then
			if(num==0) then
				fn_tensor=(.con.L%sites(rawpos)%tensor).kron.ori_tensor
			else
				fn_tensor=contract(ori_tensor,leg_nb(1:num),.con.L%sites(rawpos)%tensor,leg(1:num))
			end if
		else
			if(num==0) then
				fn_tensor=L%sites(rawpos)%tensor.kron.ori_tensor
			else
				fn_tensor=contract(ori_tensor,leg_nb(1:num),L%sites(rawpos)%tensor,leg(1:num))
			end if
		end if
	end if
	includes(rawpos)=.true.

end subroutine

subroutine contract_type(L,Tout,T1,T2,includes1,includes2)

	class(lattice),intent(inout)::L
	type(tensor),intent(inout)::Tout,T1,T2
	logical,intent(in)::includes1(:),includes2(:)
	character(len=max_char_length)::leg(30),leg_nb(30)
	integer::m,n,k,num,nb_no,rawpos,nb_rawpos
	type(tensor)::test_speed

	call L%check_unempty()
	if(all(.not.includes1))then
		Tout=T2
	else if(all(.not.includes2))then
		Tout=T1
	else
		num=0
		do m=1,L%max_site_num
			if(includes1(m))then
				do k=1,L%sites(m)%nb_num
					nb_rawpos=L%sites(m)%bonds(k)%nb_rawpos
					if(includes2(nb_rawpos)) then
						nb_no=L%sites(m)%bonds(k)%nb_no
						num=num+1
						leg(num)=L%sites(m)%bonds(k)%ind
						leg_nb(num)=L%sites(nb_rawpos)%bonds(nb_no)%ind
					end if
				end do
			end if
		end do

		if(num==0) then
			Tout=T1.kron.T2
		else
			Tout=contract(T1,leg(1:num),T2,leg_nb(1:num))
		end if
	end if

end subroutine

end module tensor_network
