MODULE tensor_network
use tensor_type
use error 
use tools
use string
use type_unidic
use mod_dictionary
implicit none

private

type bond
	private
	integer::nb_rawpos
	integer::nb_no
	type(dictionary)::info
	!logical:: env_tag=.false.
	!type(tensor)::env
	character(len=max_char_length)::ind=''
end type

type site
	private
	integer::pos(2)=0
	integer::draw_pos(2)=0
	type(bond),allocatable::bonds(:)
	character(len=max_char_length)::name=''
	type(tensor),pointer::tensor=>Null()
	!type(tensor)::tensor_save
	!type(tensor)::tensor_bac
	integer::nb_num=0
	!logical::tensor_save_tag=.false.
	!logical::back_up_tag=.false.
	!logical::con_tag=.false.
	logical::exist_tag=.false.
	type(dictionary)::info

	contains
	private
	procedure:: copy_site
	generic::assignment(=)=>copy_site
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
	procedure::set_bond_as_pure
	procedure::set_bond_as_cluster
	generic,public::set_bond_as=>set_bond_as_pure,set_bond_as_cluster

	procedure,public:: draw_l
	procedure,public:: draw_tn
	generic,public:: draw=>draw_l,draw_tn
	!final:: clean_lat

	procedure:: add_from_ten
	procedure:: add_from_lat_pos
	generic,public::add=>add_from_ten,add_from_lat_pos
	procedure:: delete_pos
	procedure:: delete_name
	generic,public::delete=>delete_pos,delete_name
	procedure:: move_pos
	procedure:: move_name
	generic,public::move=>move_pos,move_name
	procedure,public::move_row
	procedure,public::move_column
	procedure:: rename_pos
	procedure:: rename_name
	generic,public::rename=>rename_pos,rename_name

	procedure::back_up_pos
	procedure::back_up_name
	generic,public::back_up=>back_up_pos,back_up_name
	procedure::restore_pos
	procedure::restore_name
	generic,public::restore=>restore_pos,restore_name

	procedure:: get_tensor_pos
	procedure:: get_tensor_name
	generic,public:: get_tensor=>get_tensor_pos,get_tensor_name
	procedure:: get_tensor_link_pos
	procedure:: get_tensor_link_name
	generic,public:: get_tensor_link=>get_tensor_link_pos,get_tensor_link_name
	procedure:: get_env_link_pos
	procedure:: get_env_link_name
	generic,public:: get_env_link=>get_env_link_pos,get_env_link_name
	procedure:: get_env_bond_pos
	generic,public:: get_env_bond=>get_env_bond_pos
	procedure:: set_env_bond_pos
	generic,public:: set_env_bond=>set_env_bond_pos
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
	procedure:: get_bonds_pos
	procedure:: get_bonds_name
	generic,public::get_bonds=>get_bonds_pos,get_bonds_name
	procedure:: get_bond_no_pos
	procedure:: get_bond_no_name
	generic,public::get_bond_no=>get_bond_no_pos,get_bond_no_name
	procedure:: remove_bond_name
	procedure:: remove_bond_pos
	generic,public::remove_bond=>remove_bond_pos,remove_bond_name

	procedure,public:: invert_bond
	procedure:: move_nb
	procedure:: remove_nb
	procedure::get_nb_num_pos
	procedure::get_nb_num_name
	generic,public::get_nb_num=>get_nb_num_pos,get_nb_num_name
	procedure,public::get_max_nb_num
	procedure,public:: absorb

	procedure:: get_rawpos_pos
	procedure:: get_rawpos_name
	generic:: get_rawpos=>get_rawpos_pos,get_rawpos_name
	procedure,public:: get_size
	procedure,public:: get_range
	procedure:: get_max_site_num
	procedure,public:: get_name_whole
	procedure,public:: get_name_site
	generic,public::get_name=>get_name_whole,get_name_site
	procedure,public:: get_pos
	procedure,public:: set_name
	procedure,public:: ind_name
	procedure,public:: check_boundary
	procedure,public:: get_contag_pos
	procedure,public:: get_contag_name
	generic,public::get_contag=>get_contag_pos,get_contag_name
	procedure:: set_contag_pos
	procedure:: set_contag_name
	generic,public:: set_contag=>set_contag_pos,set_contag_name
	procedure,public:: cut_bonds
	procedure,public:: max_cut_bonds

	procedure,public::get_empty_tag
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


	! env related
	procedure,public:: generate_env
	procedure:: absorb_env_whole
	procedure:: absorb_env_pos_bond
	procedure:: absorb_env_name_bond
	procedure:: absorb_env_inner
	procedure:: absorb_env_pos_site
	procedure::tensor_absorb_env_pos
	generic,public::absorb_env=>absorb_env_whole
	generic,public::absorb_env_bond=>absorb_env_pos_bond,absorb_env_name_bond
	generic,public::absorb_env_site=>absorb_env_pos_site
	generic,public::tensor_absorb_env=>tensor_absorb_env_pos
	procedure:: spit_env_whole
	procedure:: spit_env_pos_bond
	procedure:: spit_env_name_bond
	procedure:: spit_env_inner
	generic,public::spit_env=>spit_env_whole
	generic,public::spit_env_bond=>spit_env_pos_bond,spit_env_name_bond

	procedure:: copy_lat
	generic,public::assignment(=)=>copy_lat

	procedure,public:: transpose_lat

	procedure:: dinsert_info_whole,zinsert_info_whole,iinsert_info_whole,linsert_info_whole,ainsert_info_whole,tinsert_info_whole
	procedure:: dinsert_info_pos,zinsert_info_pos,iinsert_info_pos,linsert_info_pos,ainsert_info_pos,tinsert_info_pos
	procedure:: dinsert_info_name,zinsert_info_name,iinsert_info_name,linsert_info_name,ainsert_info_name,tinsert_info_name
	generic,public:: insert_info=>dinsert_info_whole,dinsert_info_pos,dinsert_info_name,&
					zinsert_info_whole,zinsert_info_pos,zinsert_info_name,&
					iinsert_info_whole,iinsert_info_pos,iinsert_info_name,&
					linsert_info_whole,linsert_info_pos,linsert_info_name,&
					ainsert_info_whole,ainsert_info_pos,ainsert_info_name,&
					tinsert_info_whole,tinsert_info_pos,tinsert_info_name
	procedure,public:: dget_info_whole,zget_info_whole,iget_info_whole,lget_info_whole,aget_info_whole,tget_info_whole
	procedure,public:: dget_info_pos,zget_info_pos,iget_info_pos,lget_info_pos,aget_info_pos,tget_info_pos
	procedure,public:: dget_info_name,zget_info_name,iget_info_name,lget_info_name,aget_info_name,tget_info_name
	generic,public:: get_info=>dget_info_whole,dget_info_pos,dget_info_name,&
					zget_info_whole,zget_info_pos,zget_info_name,&
					iget_info_whole,iget_info_pos,iget_info_name,&
					lget_info_whole,lget_info_pos,lget_info_name,&
					aget_info_whole,aget_info_pos,aget_info_name,&
					tget_info_whole,tget_info_pos,tget_info_name
	procedure,public:: dset_info_whole,zset_info_whole,iset_info_whole,lset_info_whole,aset_info_whole,tset_info_whole
	procedure,public:: dset_info_pos,zset_info_pos,iset_info_pos,lset_info_pos,aset_info_pos,tset_info_pos
	procedure,public:: dset_info_name,zset_info_name,iset_info_name,lset_info_name,aset_info_name,tset_info_name
	generic,public:: set_info=>dset_info_whole,dset_info_pos,dset_info_name,&
					zset_info_whole,zset_info_pos,zset_info_name,&
					iset_info_whole,iset_info_pos,iset_info_name,&
					lset_info_whole,lset_info_pos,lset_info_name,&
					aset_info_whole,aset_info_pos,aset_info_name,&
					tset_info_whole,tset_info_pos,tset_info_name
end type   

type group
	private
	type(lattice),pointer::lat=>NULL()
	logical,allocatable::includes(:)

	contains
	private
	procedure,public:: get_info
	procedure:: draw_grp
	generic,public::draw=>draw_grp
	procedure:: empty_grp
	generic,public::empty=>empty_grp
	procedure,public:: check_can_take
	procedure:: take_pos
	procedure:: take_name
	procedure:: take_group
	generic,public:: take=>take_pos,take_name,take_group
	procedure::check_contain_pos
	procedure::check_contain_name
	generic,public:: check_contain=>check_contain_pos,check_contain_name
	procedure:: belong_group
	generic,public::belong=>belong_group
	procedure:: copy_grp
	generic,public::assignment(=)=>copy_grp
	procedure,public::get_names
	procedure::get_num_group
	generic,public::get_num=>get_num_group
	procedure,public::point_lat
	procedure::invert_bond_grp
	generic,public:: invert_bond=>invert_bond_grp
	procedure,public::get_dangle_inds

end type  

type path
	private
	integer,allocatable::raw_path(:)
	integer::num=0,current_pos=0
	type(lattice),pointer::lat=>null()

	contains
	private
	procedure::belong_path
	generic,public::belong=>belong_path
	procedure::add_name
	procedure::add_pos
	generic,public::add=>add_name,add_pos
	procedure::get_next_name
	procedure::get_next_pos
	generic,public::get_next=>get_next_name,get_next_pos
	procedure::get_order_name
	procedure::get_order_pos
	generic,public::get_order=>get_order_name,get_order_pos
	procedure::get_num_path
	generic,public::get_num=>get_num_path
	procedure::iterate_pos
	procedure::iterate_name
	generic,public::iterate=>iterate_pos,iterate_name
	procedure:: copy_path
	generic,public::assignment(=)=>copy_path
	procedure::clean_path
	generic,public::clean=>clean_path
	procedure::generate_path
	generic,public::generate=>generate_path
	procedure::draw_path
	generic,public::draw=>draw_path

end type

public lattice,group,path,lat_absorb_tensor,lat_absorb_env,lat_contract_type

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
		allocate(L%raw_pos(L%L1,L%L2))
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
		write(funit,*) S%pos
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
		read(funit,*) S%pos
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
	L%max_site_num=L1*L2
	
	allocate(L%sites(L%max_site_num))
	allocate(L%raw_pos(L1,L2))
	L%raw_pos=0
	L%empty_tag=.false.

end subroutine

subroutine generate_ten(L,D,datatype,type_)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::D
	character(len=*),intent(in)::datatype
	character(len=*),intent(in),optional::type_
	character(len=20)::type
	integer::i,k,nb_num,phy_dim

	if(present(type_)) then
		type=type_
	else
		type='rand'
	end if
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
			select case(type)
			case('rand')
				call L%sites(i)%tensor%random([-1.0d0,1.0d0])
			case('one')
				call L%sites(i)%tensor%random([1.0d0-1d-2,1.0d0+1d-2])
			case('zero')
				call L%sites(i)%tensor%setValue(0d0)
			case default
				call wc_error_stop('lattice.generate_ten','The type '//trim(type)//' is unidentified.')
			end select

			do k=1,nb_num
				call L%sites(i)%tensor%setName(k,L%sites(i)%bonds(k)%ind)
			end do
			if(phy_dim>0) call L%sites(i)%tensor%setName(nb_num+1,L%sites(i)%name+'.phy')
		end if
	end do

end subroutine

subroutine generate_env(L,type_)

	class(lattice),intent(inout) ::L
	character(len=*),optional::type_
	integer::i,k,nb_num,nb_k,nb_rawpos,D
	character(len=max_char_length)::ind,type
	type(tensor)::env

	if(present(type_)) then
		type=type_
	else
		type='rand'
	end if

	call L%check_unempty()
	do i=1,L%max_site_num
		if(L%sites(i)%exist_tag) then
			nb_num=L%sites(i)%nb_num
			do k=1,nb_num
				L%sites(i)%bonds(k)%env_tag=.true.
				nb_rawpos=L%sites(i)%bonds(k)%nb_rawpos
				if(nb_rawpos<i) then
					nb_k=L%sites(i)%bonds(k)%nb_no
					L%sites(i)%bonds(k)%env=L%sites(nb_rawpos)%bonds(nb_k)%env
				else
					ind=L%sites(i)%bonds(k)%ind
					D=L%sites(i)%tensor%dim(ind)
					select case(type)
					case('rand')
						call env%deallocate()
						call env%allocate([D,D],'real*8')
						call env%random()
						!env=eye(env)
					case('one')
						env=eye(D,D)
					end select
					L%sites(i)%bonds(k)%env=env
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

logical function get_empty_tag(L)

	class(lattice),intent(in) ::L

	get_empty_tag=L%empty_tag

end function 

logical function check_exist_pos(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	
	call L%check_unempty()
	if(pos(1)>=1 .and. pos(1)<=L%L1 .and. pos(2)>=1 .and. pos(2)<=L%L2) then
		check_exist_pos=(L%raw_pos(pos(1),pos(2))>0)
	else
		check_exist_pos=.false.
	end if

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
	else
		rawpos1=L%get_rawpos(pos1)
		rawpos2=L%get_rawpos(pos2)
		check_exist_bond_pos=(L%bonds_num_rawpos(rawpos1,rawpos2)>0)
	end if

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
	else
		rawpos1=L%get_rawpos(name1)
		rawpos2=L%get_rawpos(name2)
		check_exist_bond_name=(L%bonds_num_rawpos(rawpos1,rawpos2)>0)
	end if
	
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

	call L%check_unempty()
	call L%check_boundary(pos)
	if(L%raw_pos(pos(1),pos(2))>0)then
		call writemess(trim(str(pos))//' is not empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if
	
end subroutine

subroutine check_empty_site_name(L,name)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name

	call L%check_unempty()
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

	call L%check_boundary(pos)
	call L%check_unempty()
	if(L%raw_pos(pos(1),pos(2))==0)then
		call writemess(trim(str(pos))//' is empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if
	
end subroutine

subroutine check_unempty_site_name(L,name)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name

	call L%check_unempty()
	if(L%get_rawpos(name)==0)then
		call writemess(trim(name)//' is empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if
	
end subroutine

subroutine check_boundary(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)

	call L%check_unempty()
	if(pos(1)<1.or.pos(1)>L%L1.or.pos(2)<1.or.pos(2)>L%L2) then
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
		pass(1)=pass(1).and. all(shape(L%raw_pos)>=[L%L1,L%L2])
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
	character(len=max_char_length)::symptom
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
	else
		if(len_trim(S%name)<=0)then
			site_consistent=.false.
			symptom='Name is empty.'
		end if
		if(site_consistent)then
			if(size(S%bonds)<S%nb_num)then
				site_consistent=.false.
				symptom='Number of bonds exceeds limit.'
			end if
		end if
		if(associated(S%tensor)) then
			if(site_consistent)then
				if(.not.S%tensor%getflag())then
					site_consistent=.false.
					symptom='tensor is empty.'
				else
					do i=1,S%nb_num
						if(S%tensor%nameorder(S%bonds(i)%ind)<=0)then
							site_consistent=.false.
							symptom=trim(S%bonds(i)%ind)//' is not found.'
							exit
						end if
					end do
				end if
			end if

		end if
		if(.not. site_consistent)then
			write(*,*)'---------------------'
			write(*,*)'site is inconsistent'
			write(*,*)'symptom is: ',trim(symptom)
			write(*,*)'position is: ',S%pos
			write(*,*)'name is ',trim(S%name)
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
			write(*,*)'---------------------'
		end if
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


subroutine absorb_env_pos_site(L,pos,T)

	class(lattice),intent(inout)::L
	integer,intent(in)::pos(2)
	type(tensor),intent(inout)::T
	integer::n,rawpos,k
	type(tensor)::temp

	rawpos=L%get_rawpos(pos)
	T=L%sites(rawpos)%tensor
	if(L%sites(rawpos)%info%li('absorb_env'))then
		do k=1,L%sites(rawpos)%nb_num
				temp=L%sites(rawpos)%bonds(k)%env
				do n=1,temp%dim(1)
					call temp%setValue([n,n], dsqrt(temp%di([n,n])) )
				end do
				T=contract(T,L%sites(rawpos)%bonds(k)%ind,temp,'env.in')
				call T%setName('env.out',L%sites(rawpos)%bonds(k)%ind)
		end do
	end if

end subroutine

subroutine tensor_absorb_env_pos(L,pos1,pos2,T)

	class(lattice),intent(inout)::L
	integer,intent(in)::pos1(2),pos2(2)
	integer::n,rawpos1,rawpos2,no,nb_no
	type(tensor),intent(inout)::T
	type(tensor)::temp
	character(len=max_char_length)::ind,nb_ind

	rawpos1=L%get_rawpos(pos1)
	rawpos2=L%get_rawpos(pos2)
	call L%get_bond(pos1,no,ind,pos2,nb_no,nb_ind)
	temp=L%sites(rawpos1)%bonds(no)%env
	do n=1,temp%dim(1)
		call temp%setValue([n,n], dsqrt(temp%di([n,n])) )
	end do
	T=contract(T,L%sites(rawpos1)%bonds(no)%ind,temp,'env.in')
	call T%setName('env.out',L%sites(rawpos1)%bonds(no)%ind)

end subroutine

subroutine absorb_env_pos_bond(L,pos1,pos2)

	class(lattice),intent(inout)::L
	integer,intent(in)::pos1(2),pos2(2)
	integer::n,rawpos1,rawpos2,no,nb_no
	type(tensor)::temp
	character(len=max_char_length)::ind,nb_ind

	rawpos1=L%get_rawpos(pos1)
	rawpos2=L%get_rawpos(pos2)
	call L%get_bond(pos1,no,ind,pos2,nb_no,nb_ind)

	if(L%sites(rawpos1)%info%li('absorb_env')) call L%absorb_env_inner(rawpos1,no)
	if(L%sites(rawpos2)%info%li('absorb_env')) call L%absorb_env_inner(rawpos2,nb_no)

end subroutine

subroutine absorb_env_name_bond(L,name1,name2)

	class(lattice),intent(inout)::L
	character(len=*),intent(in)::name1,name2
	integer::n,rawpos1,rawpos2,no,nb_no
	type(tensor)::temp
	character(len=max_char_length)::ind,nb_ind

	rawpos1=L%get_rawpos(name1)
	rawpos2=L%get_rawpos(name2)
	call L%get_bond(name1,no,ind,name2,nb_no,nb_ind)

	if(L%sites(rawpos1)%info%li('absorb_env')) call L%absorb_env_inner(rawpos1,no)
	if(L%sites(rawpos2)%info%li('absorb_env')) call L%absorb_env_inner(rawpos2,nb_no)

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
	L%sites(rawpos)%tensor=contract(L%sites(rawpos)%tensor,L%sites(rawpos)%bonds(no)%ind,temp,'env.in')
	call L%sites(rawpos)%tensor%setName('env.out',L%sites(rawpos)%bonds(no)%ind)

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

subroutine spit_env_pos_bond(L,pos1,pos2)

	class(lattice),intent(inout)::L
	integer,intent(in)::pos1(2),pos2(2)
	integer::n,rawpos1,rawpos2,no,nb_no
	type(tensor)::temp
	character(len=max_char_length)::ind,nb_ind

	rawpos1=L%get_rawpos(pos1)
	rawpos2=L%get_rawpos(pos2)
	call L%get_bond(pos1,no,ind,pos2,nb_no,nb_ind)

	if(L%sites(rawpos1)%info%li('absorb_env')) call L%spit_env_inner(rawpos1,no)
	if(L%sites(rawpos2)%info%li('absorb_env')) call L%spit_env_inner(rawpos2,nb_no)

end subroutine

subroutine spit_env_name_bond(L,name1,name2)

	class(lattice),intent(inout)::L
	character(len=*),intent(in)::name1,name2
	integer::n,rawpos1,rawpos2,no,nb_no
	type(tensor)::temp
	character(len=max_char_length)::ind,nb_ind

	rawpos1=L%get_rawpos(name1)
	rawpos2=L%get_rawpos(name2)
	call L%get_bond(name1,no,ind,name2,nb_no,nb_ind)

	if(L%sites(rawpos1)%info%li('absorb_env')) call L%spit_env_inner(rawpos1,no)
	if(L%sites(rawpos2)%info%li('absorb_env')) call L%spit_env_inner(rawpos2,nb_no)

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
	L%sites(rawpos)%tensor=contract(L%sites(rawpos)%tensor,L%sites(rawpos)%bonds(no)%ind,temp,'env.in')
	call L%sites(rawpos)%tensor%setName('env.out',L%sites(rawpos)%bonds(no)%ind)

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

subroutine copy_site(S,S_old)

	class(site),intent(inout),target::S
	type(site),intent(in)::S_old

	call clean_site(S)
	S%pos=S_old%pos
	if( allocated(S%bonds))then
		if(size(S%bonds)/=size(S_old%bonds))then
			deallocate(S%bonds)
		end if
	end if
	if(.not. allocated(S%bonds)) allocate(S%bonds(size(S_old%bonds)))
	S%bonds=S_old%bonds
	S%name=S_old%name
	S%tensor_save_tag=S_old%tensor_save_tag
	S%back_up_tag=S_old%back_up_tag
	S%tensor_bac=S_old%tensor_bac
	if(S_old%tensor_save_tag)then
		S%tensor_save=S_old%tensor_save
		S%tensor=>S%tensor_save
	else
		S%tensor=>S_old%tensor
	end if
	S%nb_num=S_old%nb_num
	S%con_tag=S_old%con_tag
	S%exist_tag=S_old%exist_tag
	S%info=S_old%info

end subroutine

subroutine set_bond_as_pure(L,L_old)

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

subroutine set_bond_as_cluster(L,L_old,clusters)

	class(lattice),intent(inout)::L
	type(lattice),intent(in)::L_old
	type(lattice),target,intent(in)::clusters(:,:)
	character(len=max_char_length)::ind1,ind2,name1,name2,actual_name1,actual_name2,actual_ind1,actual_ind2
	logical::found1,found2,incl1,incl2
	integer::i,j,k,m,n,p,nb_rawpos,no,nb_no,L1,L2
	integer,allocatable::nos(:),nb_nos(:)
	character(len=max_char_length),allocatable::inds(:),nb_inds(:)
	type(lattice),pointer::belong1,belong2

	L1=size(clusters,1)
	L2=size(clusters,2)
	call L%check_unempty()
	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			do j=1,L_old%sites(i)%nb_num
				nb_rawpos=L_old%sites(i)%bonds(j)%nb_rawpos
				!write(*,*)'########',trim(L_old%sites(i)%name),'#',trim(L_old%sites(nb_rawpos)%name)
				if(nb_rawpos>i) then
					found1=.false.
					found2=.false.
					name1=L_old%sites(i)%name
					name2=L_old%sites(nb_rawpos)%name
					ind1=L_old%sites(i)%bonds(j)%ind
					nb_no=L_old%sites(i)%bonds(j)%nb_no
					ind2=L_old%sites(nb_rawpos)%bonds(nb_no)%ind
					!write(*,*)trim(ind1),' ',trim(ind2)
					if(L%check_exist(name1))then
						found1=.true.
						actual_name1=name1
						incl1=.false.
						actual_ind1=ind1
					else
						do m=1,L1
							do n=1,L2
								if( .not. clusters(m,n)%empty_tag) then
									if(clusters(m,n)%check_exist(name1))then
										actual_name1=clusters(m,n)%get_name([1,1])
										if(found1) call wc_error_stop('lattice.set_bond_as','clusters overlap!')
										found1=.true.
										incl1=.true.
										belong1=>clusters(m,n)
										!write(*,*)'belong1,',name1
										!call belong1%draw('belong1')
										if(.not. L%check_exist(actual_name1))then
											call wc_error_stop('lattice.set_bond_as','cluster of '&
												//trim(actual_name1)//' missing in lattice!')
										end if
									end if
								end if
							end do
						end do
					end if
					if(L%check_exist(name2))then
						found2=.true.
						actual_name2=name2
						incl2=.false.
						actual_ind2=ind2
					else
						do m=1,L1
							do n=1,L2
								if( .not. clusters(m,n)%empty_tag) then
									if(clusters(m,n)%check_exist(name2))then
										actual_name2=clusters(m,n)%get_name([1,1])
										if(found2) call wc_error_stop('lattice.set_bond_as','clusters overlap!')
										found2=.true.
										incl2=.true.
										belong2=>clusters(m,n)
										!write(*,*)'belong2,',name2
										!call belong2%draw('belong2')
										if(.not. L%check_exist(actual_name2))then
											call wc_error_stop('lattice.set_bond_as','cluster of '&
												//trim(actual_name2)//' missing in lattice!')
										end if
									end if
								end if
							end do
						end do
					end if
					if(found1 .and. found2)then

						if(incl1 .and. incl2)then
							if(associated(belong1,belong2))then ! in same cluster
								if(.not.belong1%check_exist_bond(name1,name2))then
									call wc_error_stop('lattice.set_bond_as','bond in cluster missing! from '&
										//trim(name1)//' to '//trim(name2))
								else 
									cycle ! do nothing
								end if
							end if
						end if

						if(incl1)then
							if(.not.belong1%check_exist_bond(actual_name1,name1))then
								call wc_error_stop('lattice.set_bond_as','bond in cluster missing! from '&
									//trim(name1)//' to '//trim(actual_name1))
							end if
							call belong1%get_bonds(actual_name1,nos,inds,name1,nb_nos,nb_inds)
							do k=1,size(nb_inds)
								!write(*,*)'-----1',trim(inds(k)),'-',trim(nb_inds(k))
								if (ind1==nb_inds(k)) actual_ind1=inds(k)
							end do
						end if

						if(incl2)then
							if(.not.belong2%check_exist_bond(actual_name2,name2))then
								call wc_error_stop('lattice.set_bond_as','bond in cluster missing! from '&
									//trim(name2)//' to '//trim(actual_name2))
							end if
							call belong2%get_bonds(actual_name2,nos,inds,name2,nb_nos,nb_inds)
							do k=1,size(nb_inds)
								!write(*,*)'-----2',trim(inds(k)),'-',trim(nb_inds(k))
								if (ind2==nb_inds(k)) actual_ind2=inds(k)
							end do
						end if
						!write(*,*)trim(actual_name1),' ',trim(actual_name2),' ',trim(actual_ind1),' ',trim(actual_ind2)
						
						if(.not. L%check_exist_bond(actual_name1,actual_name2))then
							call L%set_bond(actual_name1,actual_name2,actual_ind1,actual_ind2)
						end if
						!call L%draw('set_bond_as',check_tag=.false.)
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
	integer::i,j,k,L1_old,L2_old,pos(2),nb_pos(2),nb_rawpos,nb_no,line2_
	character(len=max_char_length)::dir,dir2,name,name2

	call L%check_unempty()
	if (all(L_old%raw_pos==0)) return ! nothing to copy

	call L_old%get_size(L1_old,L2_old)
	if(line2>=0) then 									! line2=-1 for last line
		line2_=line2
	else
		do i=L1_old,1,-1
			if(any(L_old%raw_pos(i,:)>0))then
				line2_=i+line2+1
				exit
			end if
		end do
	end if

	if(.not.(1<=line.and.line+nline-1<=L%L1)) then
		call wc_error_stop('lattice.copy_line','Input for lattice: '//trim(L%name)//' is out of range')
	end if
	if(.not.(1<=line2_.and.line2_+nline-1<=L1_old)) then
		call wc_error_stop('lattice.copy_line','Input for lattice: '//trim(L_old%get_name())//' is out of range')
	end if

	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			pos=L_old%sites(i)%pos
			if(pos(1)>=line2_ .and. pos(1)<=line2_+nline-1) then
				call L%add([pos(1)-line2_+line,pos(2)],L_old%sites(i)%name,L_old%sites(i)%tensor,L_old%sites(i)%tensor_save_tag)
				if(L_old%sites(i)%con_tag) call L%set_contag(L_old%sites(i)%name,.true.)
			end if
		end if
	end do

	do i=1,L_old%max_site_num
		if(L_old%sites(i)%exist_tag)then
			pos=L_old%sites(i)%pos
			if(pos(1)>=line2_ .and. pos(1)<=line2_+nline-1) then
				do j=1,L_old%sites(i)%nb_num
					nb_rawpos=L_old%sites(i)%bonds(j)%nb_rawpos
					nb_pos=L_old%sites(nb_rawpos)%pos
					if(nb_rawpos>i .and. nb_pos(1)>=line2_ .and. nb_pos(1)<=line2_+nline-1) then
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

subroutine back_up_pos(L,pos)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	if(.not. associated(L%sites(rawpos)%tensor)) then
		call wc_error_stop('lattice.backup','no tensor at '//str(pos))
	end if
	L%sites(rawpos)%tensor_bac=L%sites(rawpos)%tensor
	L%sites(rawpos)%back_up_tag=.true.

end subroutine

subroutine back_up_name(L,name)

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::name
	integer::rawpos

	rawpos=L%get_rawpos(name)
	if(.not. associated(L%sites(rawpos)%tensor)) then
		call wc_error_stop('lattice.backup','no tensor at '//name)
	end if
	L%sites(rawpos)%tensor_bac=L%sites(rawpos)%tensor
	L%sites(rawpos)%back_up_tag=.true.
	
end subroutine

subroutine restore_pos(L,pos)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	if(.not. associated(L%sites(rawpos)%tensor)) then
		call wc_error_stop('lattice.restore','no tensor at '//str(pos))
	end if
	if(.not. L%sites(rawpos)%back_up_tag) then
		call wc_error_stop('lattice.restore','tensor at '//trim(str(pos))//' has not been backed up before in lattice '//trim(L%name))
	end if
	L%sites(rawpos)%tensor=L%sites(rawpos)%tensor_bac
	L%sites(rawpos)%back_up_tag=.false.

end subroutine

subroutine restore_name(L,name)

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::name
	integer::rawpos

	rawpos=L%get_rawpos(name)
	if(.not. associated(L%sites(rawpos)%tensor)) then
		call wc_error_stop('lattice.restore','no tensor at '//name)
	end if
	if(.not. L%sites(rawpos)%back_up_tag) then
		call wc_error_stop('lattice.restore','tensor at '//trim(name)//' has not been backed up before')
	end if
	L%sites(rawpos)%tensor=L%sites(rawpos)%tensor_bac
	L%sites(rawpos)%back_up_tag=.false.
	
end subroutine

function get_pos(L,name) result(pos)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name
	integer::pos(2),rawpos
	
	rawpos=L%get_rawpos(name)
	pos=L%sites(rawpos)%pos

end function 

function bonds_num_pos(L,pos1,pos2) result(res)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos1(2),pos2(2)
	integer ::res
	integer::rawpos1,rawpos2

	rawpos1=L%get_rawpos(pos1)
	rawpos2=L%get_rawpos(pos2)
	res=L%bonds_num_rawpos(rawpos1,rawpos2)

end function 

function bonds_num_name(L,name1,name2) result(res)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name1,name2
	integer ::res
	integer::rawpos1,rawpos2

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

integer function get_rawpos_pos(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)

	call L%check_unempty()
	call L%check_boundary(pos)
	get_rawpos_pos=L%raw_pos(pos(1),pos(2))
	if(get_rawpos_pos<=0)then
		call writemess(trim(str(pos))//' is empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if

end function

integer function get_rawpos_name(L,name)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name

	call L%check_unempty()
	get_rawpos_name=L%name_dic%val(name)
	if(get_rawpos_name<=0)then
		call writemess(trim(name)//' is empty in Lattice '//trim(L%name))
		call wc_error_stop
	end if

end function

subroutine add_from_ten(L,pos,my_name,my_tensor,save_tag)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in) :: my_name
	type(tensor),intent(in),optional::my_tensor
	logical,intent(in),optional::save_tag
	integer::rawpos

	call L%check_unempty()

	call L%check_empty(pos)
	call L%name_dic%add(my_name,rawpos)
	L%raw_pos(pos(1),pos(2))=rawpos
	L%sites(rawpos)%pos=pos
	L%sites(rawpos)%name=my_name
	L%sites(rawpos)%exist_tag=.true.
	allocate(L%sites(rawpos)%bonds(L%max_nb_num))
	if(present(save_tag))then
		L%sites(rawpos)%tensor_save_tag=save_tag
	else
		L%sites(rawpos)%tensor_save_tag=.false.
	end if
	if(L%sites(rawpos)%tensor_save_tag) then
		L%sites(rawpos)%tensor=>L%sites(rawpos)%tensor_save
	end if
	if(present(my_tensor)) call L%set_tensor(my_name,my_tensor,L%sites(rawpos)%tensor_save_tag)

end subroutine

subroutine add_from_lat_pos(L,pos,lat,pos2,save_tag)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2),pos2(2)
	type(lattice), intent(in) :: lat
	logical,intent(in),optional::save_tag
	character(len=max_char_length)::my_name
	type(tensor),pointer::my_tensor

	call lat%check_unempty(pos2)
	my_name=lat%get_name(pos2)
	call lat%get_tensor_link(pos2,my_tensor)
	if(associated(my_tensor))then
		call L%add(pos,my_name,my_tensor,save_tag)
	else
		call L%add(pos,my_name)
	end if

end subroutine

subroutine rename_pos(L,pos,new_name)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	character(len=*), intent(in) :: new_name
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%name_dic%rename(L%sites(rawpos)%name,new_name)
	L%sites(rawpos)%name=new_name

end subroutine

subroutine rename_name(L,old_name,new_name)

	class(lattice),intent(inout) ::L
	character(len=*), intent(in) :: old_name,new_name
	integer::rawpos

	rawpos=L%get_rawpos(old_name)
	call L%name_dic%rename(old_name,new_name)
	L%sites(rawpos)%name=new_name

end subroutine

subroutine delete_pos(L,pos)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	integer::rawpos,nb_rawpos,i

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

	rawpos=L%get_rawpos(pos)
	if(rawpos/=L%raw_pos(new_pos(1),new_pos(2)))then ! do nothing when unmoved
		call L%check_empty(new_pos)
		L%raw_pos(pos(1),pos(2))=0
		L%raw_pos(new_pos(1),new_pos(2))=rawpos
		L%sites(rawpos)%pos=new_pos
	end if

end subroutine

subroutine move_row(L,row,new_row)

	class(lattice),intent(inout) ::L
	integer,intent(in)::row,new_row
	integer::j

	if(row <1 .or. row>L%L1) then
		call wc_error_stop('lattice.move_row','Source row out of range')
	end if
	if(new_row <1 .or. new_row>L%L1) then
		call wc_error_stop('lattice.move_row','Target row out of range')
	end if
	do j=1,L%L2
		if(L%check_exist([row,j])) then
			if(L%check_exist([new_row,j])) then
				call wc_error_stop('lattice.move_row','Target row occupied')
			end if
			call L%move([row,j],[new_row,j])
		end if
	end do

end subroutine

subroutine move_column(L,column,new_column)

	class(lattice),intent(inout) ::L
	integer,intent(in)::column,new_column
	integer::i

	if(column <1 .or. column>L%L2) then
		call wc_error_stop('lattice.move_column','Source column out of range')
	end if
	if(new_column <1 .or. new_column>L%L2) then
		call wc_error_stop('lattice.move_column','Target column out of range')
	end if
	do i=1,L%L2
		if(L%check_exist([i,column]))then
			if(L%check_exist([i,new_column])) then
				call wc_error_stop('lattice.move_column','Target column occupied')
			end if
			call L%move([i,column],[i,new_column])
		end if
	end do

end subroutine

subroutine move_name(L,name,new_pos)

	class(lattice),intent(inout) ::L
	character(len=*),intent(in) :: name
	integer,intent(in)::new_pos(2)
	integer::rawpos,old_pos(2)

	rawpos=L%get_rawpos(name)
	if(rawpos/=L%raw_pos(new_pos(1),new_pos(2)))then ! do nothing when unmoved
		call L%check_empty(new_pos)
		old_pos=L%sites(rawpos)%pos
		L%raw_pos(old_pos(1),old_pos(2))=0
		L%raw_pos(new_pos(1),new_pos(2))=rawpos
		L%sites(rawpos)%pos=new_pos
	end if

end subroutine

type(tensor) function get_tensor_pos(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	integer::rawpos
	
	rawpos=L%get_rawpos(pos)
	get_tensor_pos=L%sites(rawpos)%tensor

end function

type(tensor) function get_tensor_name(L,name)

	class(lattice),intent(in) ::L
	character(len=*),intent(in) :: name
	integer::rawpos
	
	rawpos=L%get_rawpos(name)
	get_tensor_name=L%sites(rawpos)%tensor

end function

subroutine get_tensor_link_pos(L,pos,tlink)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	type(tensor),pointer,intent(out)::tlink
	integer::rawpos
	
	rawpos=L%get_rawpos(pos)
	tlink=>L%sites(rawpos)%tensor

end subroutine

subroutine get_tensor_link_name(L,name,tlink)

	class(lattice),intent(in) ::L
	character(len=*),optional,intent(in)::name
	type(tensor),pointer,intent(out)::tlink
	integer::rawpos
	
	rawpos=L%get_rawpos(name)
	tlink=>L%sites(rawpos)%tensor

end subroutine

subroutine get_env_link_pos(L,pos,no,tlink)

	class(lattice),intent(in),target ::L
	integer,intent(in)::pos(2),no
	type(tensor),pointer,intent(out)::tlink
	integer::rawpos
	
	rawpos=L%get_rawpos(pos)
	if(.not.L%sites(rawpos)%bonds(no)%env_tag)then
		call wc_error_stop('lattice.get_env_link','env at '//trim(str(pos))//'-'//trim(str(no))//' does not exist')
	end if
	tlink=>L%sites(rawpos)%bonds(no)%env

end subroutine

function get_env_bond_pos(L,pos1,pos2) result(res)

	class(lattice),intent(in),target ::L
	integer,intent(in)::pos1(2),pos2(2)
	type(tensor)::res
	integer::rawpos1,rawpos2,no,nb_no
	character(len=max_char_length)::ind,nb_ind

	rawpos1=L%get_rawpos(pos1)
	rawpos2=L%get_rawpos(pos2)
	call L%get_bond(pos1,no,ind,pos2,nb_no,nb_ind)
	if(.not.L%sites(rawpos1)%bonds(no)%env_tag)then
		call wc_error_stop('lattice.get_env_link','env at '//trim(str(pos1))//'-'//trim(str(pos2))//' does not exist')
	end if
	res=L%sites(rawpos1)%bonds(no)%env

end function

subroutine set_env_bond_pos(L,pos1,pos2,env)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos1(2),pos2(2)
	type(tensor),intent(in)::env
	integer::rawpos1,rawpos2,no,nb_no
	character(len=max_char_length)::ind,nb_ind

	rawpos1=L%get_rawpos(pos1)
	rawpos2=L%get_rawpos(pos2)
	call L%get_bond(pos1,no,ind,pos2,nb_no,nb_ind)
	if(.not.L%sites(rawpos1)%bonds(no)%env_tag)then
		call wc_error_stop('lattice.get_env_link','env at '//trim(str(pos1))//'-'//trim(str(pos2))//' does not exist')
	end if
	L%sites(rawpos1)%bonds(no)%env=env
	L%sites(rawpos2)%bonds(nb_no)%env=env

end subroutine

subroutine get_env_link_name(L,name,no,tlink)

	class(lattice),intent(in),target ::L
	character(len=*),optional,intent(in)::name
	integer,intent(in)::no
	type(tensor),pointer,intent(out)::tlink
	integer::rawpos
	
	rawpos=L%get_rawpos(name)
	if(.not.L%sites(rawpos)%bonds(no)%env_tag)then
		call wc_error_stop('lattice.get_env_link','env at '//trim(name)//'-'//trim(str(no))//' does not exist')
	end if
	tlink=>L%sites(rawpos)%bonds(no)%env

end subroutine

subroutine update_tensor_pos(L,pos,new_tensor)

	class(lattice),intent(inout) ::L
	class(tensor),intent(in)::new_tensor
	integer,intent(in)::pos(2)
	integer::rawpos
	
	rawpos=L%get_rawpos(pos)
	L%sites(rawpos)%tensor=new_tensor

end subroutine

subroutine update_tensor_name(L,name,new_tensor)

	class(lattice),intent(inout) ::L
	class(tensor),intent(in)::new_tensor
	character(len=*),intent(in)::name
	integer::rawpos
		
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
		no=L%sites(rawpos)%nb_num
	else
		call wc_error_stop('set_bond','site: '//trim(L%sites(rawpos)%name)//' has reach the limit of bonds number')
	end if
	if(L%sites(rawpos2)%nb_num<L%max_nb_num)then
		L%sites(rawpos2)%nb_num=L%sites(rawpos2)%nb_num+1
		nb_no=L%sites(rawpos2)%nb_num
	else
		call wc_error_stop('set_bond','site: '//trim(L%sites(rawpos2)%name)//' has reach the limit of bonds number')
	end if

	L%sites(rawpos)%bonds(no)%nb_rawpos=rawpos2
	L%sites(rawpos)%bonds(no)%ind=dir
	L%sites(rawpos)%bonds(no)%nb_no=nb_no

	L%sites(rawpos2)%bonds(nb_no)%nb_rawpos=rawpos
	L%sites(rawpos2)%bonds(nb_no)%ind=dir2
	L%sites(rawpos2)%bonds(nb_no)%nb_no=no

end subroutine


subroutine remove_bond_pos(L,pos,pos2)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2),pos2(2)
	integer::rawpos,rawpos2

	rawpos=L%get_rawpos(pos)
	rawpos2=L%get_rawpos(pos2)

	call remove_bond_rawpos(L,rawpos,rawpos2)

end subroutine

subroutine remove_bond_name(L,name,name2)

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::name,name2
	integer::rawpos,rawpos2
	
	rawpos=L%get_rawpos(name)
	rawpos2=L%get_rawpos(name2)

	call remove_bond_rawpos(L,rawpos,rawpos2)

end subroutine

subroutine remove_bond_rawpos(L,rawpos,rawpos2)

	class(lattice),intent(inout) ::L
	integer,intent(in)::rawpos,rawpos2

	call L%remove_nb(rawpos,rawpos2)
	call L%remove_nb(rawpos2,rawpos)

end subroutine

character(len=max_char_length) function ind_name(L,pos,no) result(ind)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2),no
	integer::rawpos
	
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

subroutine get_range(L,left,right,top,bottom)

	class(lattice),intent(in) ::L
	integer,intent(out)::left,right,top,bottom
	integer::i

	if(all(L%raw_pos==0))then
		call wc_error_stop('lattice.get_range','The lattice is empty.')
	end if
	top=L%L1
	bottom=1
	do i=1,L%L1
		if(any(L%raw_pos(i,:)>0))then
			if(i<top) top=i
			if(i>bottom) bottom=i
		end if
	end do
	left=L%L2
	right=1
	do i=1,L%L2
		if(any(L%raw_pos(:,i)>0))then
			if(i<left) left=i
			if(i>right) right=i
		end if
	end do

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

	call wc_error_stop('get_bond_name','Bond from '+name+' to '+nb_name+' does not exist.')

end subroutine

subroutine get_bonds_pos(L,pos,no,ind,nb_pos,nb_no,nb_ind)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2),nb_pos(2)
	integer,allocatable,intent(out)::no(:),nb_no(:)
	character(len=max_char_length),allocatable,intent(out)::ind(:),nb_ind(:)
	integer::k,rawpos,nb_rawpos,num
	
	rawpos=L%get_rawpos(pos)
	nb_rawpos=L%get_rawpos(nb_pos)
	num=0
	do k=1,L%sites(rawpos)%nb_num
		if (L%sites(rawpos)%bonds(k)%nb_rawpos==nb_rawpos) then
			num=num+1
		end if
	end do 
	allocate(no(num))
	allocate(nb_no(num))
	allocate(ind(num))
	allocate(nb_ind(num))
	num=0
	do k=1,L%sites(rawpos)%nb_num
		if (L%sites(rawpos)%bonds(k)%nb_rawpos==nb_rawpos) then
			num=num+1
			no(num)=k
			nb_no(num)=L%sites(rawpos)%bonds(k)%nb_no
			ind(num)=L%sites(rawpos)%bonds(k)%ind
			nb_ind(num)=L%sites(nb_rawpos)%bonds(nb_no(num))%ind
		end if
	end do 

end subroutine

subroutine get_bonds_name(L,name,no,ind,nb_name,nb_no,nb_ind)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name,nb_name
	integer,allocatable,intent(out)::no(:),nb_no(:)
	character(len=max_char_length),allocatable,intent(out)::ind(:),nb_ind(:)
	integer::k,rawpos,nb_rawpos,num
	
	rawpos=L%get_rawpos(name)
	nb_rawpos=L%get_rawpos(nb_name)
	num=0
	do k=1,L%sites(rawpos)%nb_num
		if (L%sites(rawpos)%bonds(k)%nb_rawpos==nb_rawpos) then
			num=num+1
		end if
	end do 
	allocate(no(num))
	allocate(nb_no(num))
	allocate(ind(num))
	allocate(nb_ind(num))
	num=0
	do k=1,L%sites(rawpos)%nb_num
		if (L%sites(rawpos)%bonds(k)%nb_rawpos==nb_rawpos) then
			num=num+1
			no(num)=k
			nb_no(num)=L%sites(rawpos)%bonds(k)%nb_no
			ind(num)=L%sites(rawpos)%bonds(k)%ind
			nb_ind(num)=L%sites(nb_rawpos)%bonds(nb_no(num))%ind
		end if
	end do 

end subroutine

subroutine get_bond_no_pos(L,pos,no,ind,nb_pos,nb_no,nb_ind)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2),no
	integer,intent(out)::nb_no,nb_pos(2)
	character(len=max_char_length),intent(out)::ind,nb_ind
	integer::k,rawpos,nb_rawpos
	
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

	rawpos=L%get_rawpos(pos)
	get_name_site=L%sites(rawpos)%name

end function

subroutine set_name(L,my_name)

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::my_name

	call L%check_unempty()
	L%name=my_name
	
end subroutine

integer function get_max_nb_num(L)

	class(lattice),intent(in) ::L

	get_max_nb_num=L%max_nb_num

end function

integer function get_nb_num_pos(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	get_nb_num_pos=L%sites(rawpos)%nb_num

end function

integer function get_nb_num_name(L,name)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name
	integer::rawpos

	rawpos=L%get_rawpos(name)
	get_nb_num_name=L%sites(rawpos)%nb_num

end function

logical function get_contag_pos(L,pos)

	class(lattice),intent(in) ::L
	integer,intent(in)::pos(2)
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	get_contag_pos=L%sites(rawpos)%con_tag

end function

logical function get_contag_name(L,name)

	class(lattice),intent(in) ::L
	character(len=*),intent(in)::name
	integer::rawpos

	rawpos=L%get_rawpos(name)
	get_contag_name=L%sites(rawpos)%con_tag

end function

subroutine set_contag_pos(L,pos,status)

	class(lattice),intent(inout) ::L
	integer,intent(in)::pos(2)
	logical,intent(in)::status
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	L%sites(rawpos)%con_tag=status

end subroutine

subroutine set_contag_name(L,name,status)

	class(lattice),intent(inout) ::L
	character(len=*),intent(in)::name
	logical,intent(in)::status
	integer::rawpos

	rawpos=L%get_rawpos(name)
	L%sites(rawpos)%con_tag=status

end subroutine

subroutine invert_bond(L,pos,temp)

	class(lattice),intent(in)::L
	class(tensor),intent(inout)::temp
	integer,intent(in)::pos(2)
	integer::no,nb_pos(2),nb_no,rawpos,nb_rawpos

	rawpos=L%get_rawpos(pos)
	do no=1,L%sites(rawpos)%nb_num
		nb_rawpos=L%sites(rawpos)%bonds(no)%nb_rawpos
		nb_no=L%sites(rawpos)%bonds(no)%nb_no
		call temp%setName(L%sites(nb_rawpos)%bonds(nb_no)%ind,L%sites(rawpos)%bonds(no)%ind)
	end do

end subroutine

integer function cut_bonds(L,i,range) !bond between ith and i+1th line

	class(lattice),intent(in)::L
	integer,intent(in)::i
	integer,intent(in),optional::range
	integer::j,k,no,L1,L2,rawpos,nb_rawpos,nb_pos(2),jfrom,jto

	if(present(range))then
		jfrom=max(1,i-range+1)
		jto=i+range
	else
		jfrom=i
		jto=i+1
	end if
	call L%check_unempty()
	call check_boundary(L,[i,1])
	cut_bonds=0
	do j=jfrom,i
		do k=1,L%L2
			rawpos=L%raw_pos(j,k)
			if(rawpos>0)then
				do no=1,L%sites(rawpos)%nb_num
					nb_rawpos=L%sites(rawpos)%bonds(no)%nb_rawpos
					if(L%sites(nb_rawpos)%pos(1)>i .and. L%sites(nb_rawpos)%pos(1)<=jto)then	
						! essential to pbc and cyl
						cut_bonds=cut_bonds+1
					end if
				end do
			end if
		end do		
	end do	

end function 

integer function max_cut_bonds(L,range)

	class(lattice),intent(in)::L
	integer,intent(in),optional::range
	integer::i

	max_cut_bonds=0
	do i=1,L%L1-1
		max_cut_bonds=max(max_cut_bonds,L%cut_bonds(i,range))
	end do

end function 

!!!!!!!!!!!!!!!!!!!!!!!!! info !!!!!!!!!!!!!!!!!!!!!!!!!!!!


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

	rawpos=L%get_rawpos(pos)
	dic=>L%sites(rawpos)%info

end subroutine

subroutine point_info_name(L,name,dic)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	type(dictionary),pointer, intent(inout) :: dic
	integer::rawpos

	rawpos=L%get_rawpos(name)
	dic=>L%sites(rawpos)%info

end subroutine

subroutine dget_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	real(8), intent(inout) :: val

	call L%check_unempty()
	call L%info%getvalue(val_name,val)

end subroutine

subroutine zget_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	complex(8), intent(inout) :: val

	call L%check_unempty()
	call L%info%getvalue(val_name,val)

end subroutine

subroutine iget_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	integer, intent(inout) :: val

	call L%check_unempty()
	call L%info%getvalue(val_name,val)

end subroutine

subroutine lget_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	logical, intent(inout) :: val

	call L%check_unempty()
	call L%info%getvalue(val_name,val)

end subroutine

subroutine aget_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	character(len=max_char_length), intent(inout) :: val

	call L%check_unempty()
	call L%info%getvalue(val_name,val)

end subroutine

subroutine tget_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	type(tensor), intent(inout) :: val

	call L%check_unempty()
	call L%info%getvalue(val_name,val)

end subroutine

subroutine dget_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	real(8), intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine zget_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	complex(8), intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine iget_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	integer, intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine lget_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	logical, intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine aget_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	character(len=max_char_length), intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine tget_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	type(tensor), intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine dget_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	real(8), intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine zget_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	complex(8), intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine iget_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	integer, intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine lget_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	logical, intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine aget_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	character(len=max_char_length), intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine tget_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	type(tensor), intent(inout) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%getvalue(val_name,val)

end subroutine

subroutine dset_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	real(8), intent(in) :: val

	call L%check_unempty()
	call L%info%setvalue(val_name,val)

end subroutine

subroutine zset_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	complex(8), intent(in) :: val

	call L%check_unempty()
	call L%info%setvalue(val_name,val)

end subroutine

subroutine iset_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	integer, intent(in) :: val

	call L%check_unempty()
	call L%info%setvalue(val_name,val)

end subroutine

subroutine lset_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	logical, intent(in) :: val

	call L%check_unempty()
	call L%info%setvalue(val_name,val)

end subroutine

subroutine aset_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	character(len=*), intent(in) :: val

	call L%check_unempty()
	call L%info%setvalue(val_name,val)

end subroutine

subroutine tset_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	type(tensor), intent(in) :: val

	call L%check_unempty()
	call L%info%setvalue(val_name,val)

end subroutine

subroutine dset_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	real(8), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine zset_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	complex(8), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine iset_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	integer, intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine lset_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	logical, intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine aset_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	character(len=max_char_length), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine tset_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	type(tensor), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine dset_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	real(8), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine zset_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	complex(8), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine iset_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	integer, intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine lset_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	logical, intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine aset_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	character(len=max_char_length), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine tset_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	type(tensor), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%setvalue(val_name,val)

end subroutine

subroutine dinsert_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	real(8), intent(in) :: val

	call L%check_unempty()
	call L%info%insert(val_name,val)

end subroutine

subroutine zinsert_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	complex(8), intent(in) :: val

	call L%check_unempty()
	call L%info%insert(val_name,val)

end subroutine

subroutine iinsert_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	integer, intent(in) :: val

	call L%check_unempty()
	call L%info%insert(val_name,val)

end subroutine

subroutine linsert_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	logical, intent(in) :: val

	call L%check_unempty()
	call L%info%insert(val_name,val)

end subroutine

subroutine ainsert_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	character(len=max_char_length), intent(in) :: val

	call L%check_unempty()
	call L%info%insert(val_name,val)

end subroutine

subroutine tinsert_info_whole(L,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::val_name
	type(tensor), intent(in) :: val

	call L%check_unempty()
	call L%info%insert(val_name,val)

end subroutine

subroutine dinsert_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	real(8), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

subroutine zinsert_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	complex(8), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

subroutine iinsert_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	integer, intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

subroutine linsert_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	logical, intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

subroutine ainsert_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	character(len=max_char_length), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

subroutine tinsert_info_name(L,name,val_name,val)

	class(lattice),intent(inout),target ::L
	character(len=*),intent(in)::name
	character(len=*),intent(in)::val_name
	type(tensor), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(name)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

subroutine dinsert_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	real(8), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

subroutine zinsert_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	complex(8), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

subroutine iinsert_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	integer, intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

subroutine linsert_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	logical, intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

subroutine ainsert_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	character(len=max_char_length), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

subroutine tinsert_info_pos(L,pos,val_name,val)

	class(lattice),intent(inout),target ::L
	integer,intent(in)::pos(2)
	character(len=*),intent(in)::val_name
	type(tensor), intent(in) :: val
	integer::rawpos

	rawpos=L%get_rawpos(pos)
	call L%sites(rawpos)%info%insert(val_name,val)

end subroutine

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
	character(len=max_char_length)::leg(20),leg_nb(20)
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

subroutine draw_l(L,filename,label_bond,fixed,check_tag,stop_tag)

	class(lattice),intent(in)::L
	character(len=*),intent(in),optional::filename
	logical,intent(in),optional::label_bond,fixed,check_tag,stop_tag
	integer::i

	if(present(filename)) then
		call L%draw_tn(trim(filename),'',[(.false.,i=1,L%max_site_num)],[(-1,i=1,L%max_site_num)]&
			,label_bond,fixed,check_tag,stop_tag)
	else
		call L%draw_tn(trim(L%name),'',[(.false.,i=1,L%max_site_num)],[(-1,i=1,L%max_site_num)]&
			,label_bond,fixed,check_tag,stop_tag)
	end if

end subroutine

subroutine draw_tn(L,filename,tnname,includes,path,label_bond_,fixed_,check_tag_,stop_tag_)
use mod_mpi_info

	class(lattice),intent(in)::L
	character(len=*),intent(in)::filename
	character(len=*),intent(in)::tnname
	logical,intent(in)::includes(:)
	integer,intent(in)::path(:)
	logical,intent(in),optional::label_bond_,fixed_,check_tag_,stop_tag_
	logical::label_bond,fixed,check_tag,stop_tag
	character(len=200)::command
	integer::i,j,k,nb,pos(2),nb_no,nattr

	if(present(label_bond_)) then
		label_bond=label_bond_
	else
		label_bond=.true.
	end if
	if(present(fixed_)) then
		fixed=fixed_
	else
		fixed=.true.
	end if
	if(present(check_tag_)) then
		check_tag=check_tag_
	else
		check_tag=.true.
	end if
	if(present(stop_tag_)) then
		stop_tag=stop_tag_
	else
		stop_tag=.true.
	end if

	if(L%empty_tag) then
		call writemess('-----------------------------------------------')
		call writemess('empty lattice, do not draw anything')
		call writemess('-----------------------------------------------')
		call L%check_consistency()
		return
	end if

	if(my_rank==0)then

		open(unit=7878,file='./'//trim(filename)//'.dot')
		write(7878,*)'digraph G {'
		if(len_trim(tnname)>0)then
			write(7878,*) '  label = "'//trim(tnname)//' in '//trim(L%name)//'"'
		else
			write(7878,*) '  label = "'//trim(L%name)//'"'
		end if
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

				!write(*,*)L%sites(i)%name,'---',L%sites(i)%pos

				if(all(pos>0))then
					if(fixed)then
						command=command+', pos = "'+(3*pos(2))+','+(-pos(1))+'" ,pin="true"]'
					else
						command=command+']'
					end if
				end if
				write(7878,'(A)')trim(command)
			end if
		end do
		write(7878,*)'subgraph bond {'
		write(7878,*)'edge[dir=none]'
		do i=1,L%max_site_num
			if(L%sites(i)%exist_tag) then
				do k=1,L%sites(i)%nb_num
					nb=L%sites(i)%bonds(k)%nb_rawpos
					nb_no=L%sites(i)%bonds(k)%nb_no
					if(nb>i)then
						nattr=0
						command='  node_'+i+' -> node_'+nb+' ['
						if(label_bond)then
							command=command+'taillabel = "'+after_dot(L%sites(i)%bonds(k)%ind) &
							+'",headlabel ="'+after_dot(L%sites(nb)%bonds(nb_no)%ind)+'"'
							nattr=nattr+1
						end if
						if(includes(i).and.includes(nb)) then
							if (nattr>0) command=command+', '
							command=command+'color="red"'
							nattr=nattr+1
						end if
						if(.not. fixed)then
							if (nattr>0) command=command+', '
							command=command+'len=3'
							nattr=nattr+1
						end if
						command=command+']'
						write(7878,'(A)')trim(command)
					end if
				end do
			end if
		end do
		write(7878,*)'}'
		write(7878,*)'subgraph path {'

		do i=1,count(path>0)-1
			nattr=0
			command='  node_'+path(i)+' -> node_'+path(i+1)+' [color="blue",style="dashed",penwidth=2.0]'
			write(7878,'(A)')trim(command)
		end do
		write(7878,*)'}'

		write(7878,*) '}'

		close(7878)
		call system('neato -Tpng -Gdpi=200 ./'//trim(filename)//'.dot > ./'//trim(filename)//'.png')
#if DRAW_SHOW
		call system('./show.sh ./'//trim(filename)//'.png')
#endif
#if DRAW_ENVICE
		call system('evince ./'//trim(filename)//'.png')
#endif
		!call system('rm ./'//trim(filename)//'.dot')
		if(check_tag) call L%check_consistency()
		if(stop_tag) then
			call writemess('The program has been paused. Please press enter to continue')
			read(*,*)
		end if
	end if

end subroutine

!!!!!!!!!!!!!!!!!!!!!!! Used in tn_tensor !!!!!!!!!!!!!!!!!!!!!!

subroutine lat_absorb_tensor(fn_tensor,ori_tensor,G,pos)

	type(group),intent(inout)::G
	type(tensor),target,intent(inout)::ori_tensor
	type(tensor),target,intent(inout)::fn_tensor
	integer,intent(in)::pos(2)
	character(len=max_char_length)::leg(20),leg_nb(20)
	integer::k,num,nb(2),nb_no,rawpos,nb_rawpos
	type(tensor)::testen
	type(tensor),pointer::tpf,tpo
	complex(8),pointer::tendata(:)

	call G%lat%check_unempty()
	rawpos=G%lat%raw_pos(pos(1),pos(2))
	if(rawpos<=0) return
	tpf=>fn_tensor
	tpo=>ori_tensor

	if(all(.not.G%includes))then
		if(G%lat%sites(rawpos)%con_tag) then
			fn_tensor=.con.G%lat%sites(rawpos)%tensor
		else
			fn_tensor=G%lat%sites(rawpos)%tensor
		end if
	else if(.not. G%includes(rawpos)) then
		num=0
		do k=1,G%lat%sites(rawpos)%nb_num
			nb_rawpos=G%lat%sites(rawpos)%bonds(k)%nb_rawpos
			nb_no=G%lat%sites(rawpos)%bonds(k)%nb_no
			if(G%includes(nb_rawpos)) then
				num=num+1
				leg(num)=G%lat%sites(rawpos)%bonds(k)%ind
				leg_nb(num)=G%lat%sites(nb_rawpos)%bonds(nb_no)%ind
			end if
		end do

		if(G%lat%sites(rawpos)%con_tag) then
			if(num==0) then
				fn_tensor=(.con.G%lat%sites(rawpos)%tensor).kron.ori_tensor
			else
				if(associated(tpf,tpo))then
					call fn_tensor%contract(leg_nb(1:num),.con.G%lat%sites(rawpos)%tensor,leg(1:num))
				else
					fn_tensor=contract(ori_tensor,leg_nb(1:num),.con.G%lat%sites(rawpos)%tensor,leg(1:num))
				end if
			end if
		else
			if(num==0) then
				fn_tensor=G%lat%sites(rawpos)%tensor.kron.ori_tensor
			else
				if(associated(tpf,tpo))then
					call fn_tensor%contract(leg_nb(1:num),G%lat%sites(rawpos)%tensor,leg(1:num))
				else
					fn_tensor=contract(ori_tensor,leg_nb(1:num),G%lat%sites(rawpos)%tensor,leg(1:num))
				end if
			end if
		end if
	end if
	call G%take(pos)

end subroutine

subroutine lat_absorb_env(fn_tensor,ori_tensor,G,pos)

	type(group),intent(inout)::G
	type(tensor),target,intent(inout)::ori_tensor
	type(tensor),target,intent(inout)::fn_tensor
	integer,intent(in)::pos(2)
	character(len=max_char_length)::nb_ind
	integer::k,num,nb_no,rawpos,nb_rawpos
	type(tensor)::env
	type(tensor),pointer::tpf,tpo

	call G%lat%check_unempty()
	rawpos=G%lat%raw_pos(pos(1),pos(2))
	if(rawpos<=0) return
	tpf=>fn_tensor
	tpo=>ori_tensor

	if(any(G%includes).and.(.not. G%includes(rawpos))) then
		do k=1,G%lat%sites(rawpos)%nb_num
			nb_rawpos=G%lat%sites(rawpos)%bonds(k)%nb_rawpos
			nb_no=G%lat%sites(rawpos)%bonds(k)%nb_no
			nb_ind=G%lat%sites(nb_rawpos)%bonds(nb_no)%ind
			if(G%includes(nb_rawpos)) then
				if(G%lat%sites(nb_rawpos)%bonds(nb_no)%env_tag)then
					env=G%lat%sites(nb_rawpos)%bonds(nb_no)%env
					if(associated(tpf,tpo))then
						call fn_tensor%contract(nb_ind,env,'env.in')
					else
						fn_tensor=contract(ori_tensor,nb_ind,env,'env.in')
					end if
					call fn_tensor%setName('env.out',nb_ind)
				end if
			end if
		end do
	end if

end subroutine

subroutine lat_contract_type(Tout,T1,T2,G1,G2)	! G1 will take G2

	type(group),intent(inout)::G1,G2
	type(tensor),intent(inout)::Tout,T1,T2
	character(len=max_char_length)::leg(30),leg_nb(30)
	integer::m,n,k,num,nb_no,rawpos,nb_rawpos
	type(tensor)::test_speed

	call G1%check_can_take(G2)
	call G1%lat%check_unempty()
	if(all(.not.G1%includes))then
		Tout=T2
	else if(all(.not.G2%includes))then
		Tout=T1
	else if(any(G1%includes .and. G2%includes))then
		call wc_error_stop('lattice.contract_type','Two includes shound not overlap!')
	else
		num=0
		do m=1,G1%lat%max_site_num
			if(G1%includes(m))then
				do k=1,G1%lat%sites(m)%nb_num
					nb_rawpos=G1%lat%sites(m)%bonds(k)%nb_rawpos
					if(G2%includes(nb_rawpos)) then
						nb_no=G1%lat%sites(m)%bonds(k)%nb_no
						num=num+1
						leg(num)=G1%lat%sites(m)%bonds(k)%ind
						leg_nb(num)=G1%lat%sites(nb_rawpos)%bonds(nb_no)%ind
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

	call G1%take(G2)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!! Group !!!!!!!!!!!!!!!!!!!!!!!!


function check_contain_pos(G,pos) result(ct)

	class(group),intent(in)::G
	integer,intent(in)::pos(2)
	integer::rawpos
	logical :: ct

	ct=.false.
	if(associated(G%lat))then
		if(G%lat%check_exist(pos))then
			rawpos=G%lat%get_rawpos(pos)
			ct=G%includes(rawpos)
		end if
	end if

end function

function check_contain_name(G,name) result(ct)

	class(group),intent(in)::G
	character(len=*),intent(in)::name
	integer::rawpos
	logical :: ct

	ct=.false.
	if(associated(G%lat))then
		if(G%lat%check_exist(name))then
			rawpos=G%lat%get_rawpos(name)
			ct=G%includes(rawpos)
		end if
	end if

end function

subroutine copy_grp(G1,G2)

	class(group),intent(inout)::G1
	type(group),intent(in)::G2

	if(associated(G2%lat))then
		call G1%belong(G2%lat)
		deallocate(G1%includes)
		G1%includes=G2%includes
	end if

end subroutine

subroutine belong_group(G,L)

	class(group),intent(inout)::G
	type(lattice),target,intent(in) ::L
	integer::num

	G%lat=>L

	num=max(L%get_max_site_num(),1)
	if(allocated(G%includes))then
		if(size(G%includes)/=num) then
			deallocate(G%includes)
			allocate(G%includes(num))
		end if
	else
		allocate(G%includes(num))
	end if
	G%includes=.false.

end subroutine

subroutine empty_grp(G)

	class(group),intent(inout)::G

	if(allocated(G%includes)) G%includes=.false.

end subroutine

subroutine draw_grp(G,filename,tnname,label_bond,fixed,check_tag)

	class(group),intent(in)::G
	character(len=*),intent(in)::tnname,filename
	logical,intent(in),optional::label_bond,fixed,check_tag
	integer::i

	if(.not.associated(G%lat)) then
		call writemess('-------------------------------------------------------')
		call writemess('Group not belong to any lattice, do not draw anything')
		call writemess('-------------------------------------------------------')
		return
	end if
	call G%lat%draw(filename,tnname,G%includes,[(-1,i=1,G%lat%max_site_num)],label_bond,fixed,check_tag)

end subroutine

subroutine get_info(G)

	class(group),intent(in)::G
	integer::L1,L2

	call G%lat%get_size(L1,L2)
	call writemess('The group is in lattice :'+G%lat%get_name())
	call writemess('with sites included are')
	write(*,'(L2)') G%includes

end subroutine

subroutine take_pos(G,pos)	!if already includes or pos have no site, don't do anything

	class(group),intent(inout)::G
	integer,intent(in)::pos(2)

	if(G%lat%check_exist(pos))then
		G%includes(G%lat%get_rawpos(pos))=.true.
	end if

end subroutine

subroutine take_name(G,name)	!if already includes or pos have no site, don't do anything

	class(group),intent(inout)::G
	character(len=*),intent(in)::name

	if(G%lat%check_exist(name))then
		G%includes(G%lat%get_rawpos(name))=.true.
	end if

end subroutine

subroutine take_group(G,G2)

	class(group),intent(inout)::G
	class(group),intent(in)::G2

	call G%check_can_take(G2)
	G%includes = G%includes .or. G2%includes

end subroutine

subroutine check_can_take(G,G2)

	class(group),intent(in)::G
	class(group),intent(in)::G2

	if(.not. associated(G%lat,G2%lat))then
		call wc_error_stop('group.check_can_take','Groups not in the same lattice.')
	end if
	if(any(G%includes .and. G2%includes))then
		call wc_error_stop('group.check_can_take','Groups overlap.')
	end if

end subroutine

subroutine point_lat(G,L)

	class(group),target,intent(in)::G
	type(lattice),pointer,intent(inout)::L

	L => G%lat

end subroutine

function get_num_group(G) result(num)

	class(group),intent(in)::G
	integer::num

	num=count(G%includes)

end function

function get_names(G) result(names)

	class(group),intent(in)::G
	character(len=max_char_length),allocatable::names(:)
	integer::num,i,n

	num=count(G%includes)
	allocate(names(num))
	n=1
	do i=1,size(G%includes)
		if(G%includes(i))then
			names(n)=G%lat%sites(i)%name
			n=n+1
		end if
	end do

end function

subroutine invert_bond_grp(G,T)

	class(group),intent(in)::G
	class(tensor),intent(inout)::T
	integer::i,j,nb_i,nb_j

	if(.not. associated(G%lat))then
		call wc_error_stop('group.invert_bond','lattice not associated')
	end if
	call G%lat%check_unempty()
	do i=1,G%lat%max_site_num
		if(G%includes(i))then
			do j=1,G%lat%sites(i)%nb_num
				nb_i=G%lat%sites(i)%bonds(j)%nb_rawpos
				if(.not.G%includes(nb_i))then
					nb_j=G%lat%sites(i)%bonds(j)%nb_no
					call T%setName(G%lat%sites(i)%bonds(j)%ind,G%lat%sites(nb_i)%bonds(nb_j)%ind)
				end if
			end do
		end if
	end do

end subroutine

function get_dangle_inds(G,pos) result(inds)

	class(group),intent(in)::G
	integer,intent(in)::pos(2)
	character(len=max_char_length),allocatable::inds(:)
	integer::num,j,rawpos,nb_rawpos

	rawpos=G%lat%get_rawpos(pos)
	num=0
	do j=1,G%lat%sites(rawpos)%nb_num
		nb_rawpos=G%lat%sites(rawpos)%bonds(j)%nb_rawpos
		if(.not.G%includes(nb_rawpos)) num=num+1
	end do

	allocate(inds(num))
	num=0
	do j=1,G%lat%sites(rawpos)%nb_num
		nb_rawpos=G%lat%sites(rawpos)%bonds(j)%nb_rawpos
		if(.not.G%includes(nb_rawpos))then
			num=num+1
			inds(num)=G%lat%sites(rawpos)%bonds(j)%ind
		end if
	end do

end function

!path

subroutine belong_path(P,L)

	class(path),intent(inout)::P
	type(lattice),target,intent(in) ::L
	integer::num

	P%lat=>L

	num=max(L%get_max_site_num(),1)
	if(allocated(P%raw_path))then
		if(size(P%raw_path)/=num) then
			deallocate(P%raw_path)
			allocate(P%raw_path(num))
		end if
	else
		allocate(P%raw_path(num))
	end if
	P%raw_path=-1
	P%num=0

end subroutine

subroutine add_name(P,name)

	class(path),intent(inout)::P
	character(len=*),intent(in)::name
	integer::rawpos

	if(.not. associated(P%lat))then
		call wc_error_stop('path.add','path not belong to any lattice')
	end if
	rawpos=P%lat%get_rawpos(name)
	if(any(P%raw_path(1:P%num)==rawpos))then
		call wc_error_stop('path.add','path already contains '//trim(name))
	end if
	P%num=P%num+1
	P%raw_path(P%num)=rawpos

end subroutine

subroutine add_pos(P,pos)

	class(path),intent(inout)::P
	integer,intent(in)::pos(2)
	integer::rawpos

	if(.not. associated(P%lat))then
		call wc_error_stop('path.add','path not belong to any lattice')
	end if
	rawpos=P%lat%get_rawpos(pos)
	if(any(P%raw_path(1:P%num)==rawpos))then
		call wc_error_stop('path.add','path already contains '//trim(str(pos)))
	end if
	P%num=P%num+1
	P%raw_path(P%num)=rawpos
	
end subroutine

function get_order_name(P,name) result(order)

	class(path),intent(inout)::P
	character(len=*),intent(in)::name
	integer::order
	logical::found
	integer::rawpos

	if(.not. associated(P%lat))then
		call wc_error_stop('path.add','path not belong to any lattice')
	end if
	rawpos=P%lat%get_rawpos(name)
	found=.false.
	do order=1,P%num
		if(P%raw_path(order)==rawpos)then
			found=.true.
			exit
		end if
	end do
	if (.not. found)then
		call wc_error_stop('path.get_order',trim(name)//' is not found in the path.')
	end if

end function

function get_order_pos(P,pos) result(order)

	class(path),intent(inout)::P
	integer,intent(in)::pos(2)
	integer::order
	logical::found
	integer::rawpos

	if(.not. associated(P%lat))then
		call wc_error_stop('path.add','path not belong to any lattice')
	end if
	rawpos=P%lat%get_rawpos(pos)
	found=.false.
	do order=1,P%num
		if(P%raw_path(order)==rawpos)then
			found=.true.
			exit
		end if
	end do
	if (.not. found)then
		call wc_error_stop('path.get_order',trim(str(pos))//' is not found in the path.')
	end if

end function

subroutine get_next_pos(P,this_pos,next_pos)

	class(path),intent(inout)::P
	integer,intent(in)::this_pos(2)
	integer,intent(out)::next_pos(2)
	integer::this_order

	this_order=P%get_order(this_pos)
	if(this_order==P%num)then
		call wc_error_stop('path.get_next',trim(str(this_pos))//' is the last one.')
	end if
	next_pos=P%lat%sites(P%raw_path(this_order)+1)%pos

end subroutine

subroutine get_next_name(P,this_name,next_name)

	class(path),intent(inout)::P
	character(len=*),intent(in)::this_name
	character(len=*),intent(out)::next_name
	integer::this_order

	this_order=P%get_order(this_name)
	if(this_order==P%num)then
		call wc_error_stop('path.get_next',trim(this_name)//' is the last one.')
	end if
	next_name=P%lat%sites(P%raw_path(this_order)+1)%name

end subroutine

subroutine iterate_pos(P,pos,first_tag)

	class(path),intent(inout)::P
	logical,intent(in)::first_tag
	integer,intent(out)::pos(2)

	if(.not. associated(P%lat))then
		call wc_error_stop('path.iterate','path not belong to any lattice')
	end if
	if(P%num==0)then
		call wc_error_stop('path.iterate','No sites in the path.')
	end if

	if(first_tag)then
		P%current_pos=1
		pos=P%lat%sites(P%raw_path(P%current_pos))%pos
	else
		if(P%current_pos==P%num)then
			call wc_error_stop('path.iterate','Already reached the last one.')
		end if
		P%current_pos=P%current_pos+1
		pos=P%lat%sites(P%raw_path(P%current_pos))%pos
	end if

end subroutine

subroutine iterate_name(P,name,first_tag)

	class(path),intent(inout)::P
	logical,intent(in)::first_tag
	character(len=max_char_length),intent(out)::name

	if(.not. associated(P%lat))then
		call wc_error_stop('path.iterate','path not belong to any lattice')
	end if
	if(P%num==0)then
		call wc_error_stop('path.iterate','No sites in the path.')
	end if

	if(first_tag)then
		P%current_pos=1
		name=P%lat%sites(P%raw_path(P%current_pos))%name
	else
		if(P%current_pos==P%num)then
			call wc_error_stop('path.iterate','Already reached the last one.')
		end if
		P%current_pos=P%current_pos+1
		name=P%lat%sites(P%raw_path(P%current_pos))%name
	end if

end subroutine

function get_num_path(P) result(num)

	class(path),intent(inout)::P
	integer::num

	num=P%num

end function

subroutine copy_path(P1,P2)

	class(path),intent(inout)::P1
	type(path),intent(in)::P2

	if(associated(P2%lat))then
		call P1%belong(P2%lat)
		deallocate(P1%raw_path)
		P1%raw_path=P2%raw_path
		P1%num=P2%num
		P1%current_pos=P2%current_pos
	end if

end subroutine

subroutine clean_path(P)

	class(path),intent(inout)::P

	if(associated(P%lat))then
		P%num=0
		P%current_pos=0
		P%raw_path=-1
	end if

end subroutine

subroutine generate_path(P,dir,avoid)

	class(path),intent(inout)::P
	character(len=*),intent(in)::dir
	type(group),optional,intent(in)::avoid
	integer::L1,L2,m,n

	call P%lat%get_size(L1,L2)
	call P%clean()
	select case(dir)
	case('lu')
		do n=1,L2
			do m=1,L1
				if(P%lat%check_exist([m,n]))then
					if(present(avoid))then
						if(.not. avoid%check_contain([m,n]))then
							call P%add([m,n])
						end if
					else
						call P%add([m,n])
					end if
				end if
			end do
		end do
	case('ru')
		do n=L2,1,-1
			do m=1,L1
				if(P%lat%check_exist([m,n]))then
					if(present(avoid))then
						if(.not. avoid%check_contain([m,n]))then
							call P%add([m,n])
						end if
					else
						call P%add([m,n])
					end if
				end if
			end do
		end do
	case('ld')
		do n=1,L2
			do m=L1,1,-1
				if(P%lat%check_exist([m,n]))then
					if(present(avoid))then
						if(.not. avoid%check_contain([m,n]))then
							call P%add([m,n])
						end if
					else
						call P%add([m,n])
					end if
				end if
			end do
		end do
	case('rd')
		do n=L2,1,-1
			do m=L1,1,-1
				if(P%lat%check_exist([m,n]))then
					if(present(avoid))then
						if(.not. avoid%check_contain([m,n]))then
							call P%add([m,n])
						end if
					else
						call P%add([m,n])
					end if
				end if
			end do
		end do
	end select

end subroutine

subroutine draw_path(P,filename,label_bond,fixed,check_tag)

	class(path),intent(in)::P
	character(len=*),intent(in)::filename
	logical,intent(in),optional::label_bond,fixed,check_tag
	integer::i

	if(.not.associated(P%lat)) then
		call writemess('-------------------------------------------------------')
		call writemess('Path not belong to any lattice, do not draw anything')
		call writemess('-------------------------------------------------------')
		return
	end if

	call P%lat%draw_tn(filename,filename,[(.false.,i=1,P%lat%max_site_num)],P%raw_path,label_bond,fixed,check_tag)

end subroutine

end module
