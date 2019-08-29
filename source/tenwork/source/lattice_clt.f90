module tensor_network_nesting
use tensor_network
use tensor_type
use error 
use tools
use string
use tn_tensor_type
implicit none

private

type,extends(lattice):: nest_lattice

	private
	class(lattice),pointer,public::lat_pre	! old lattice
	type(tensor),allocatable::ten_res(:,:)	! results of clusters 
	type(tensor),allocatable::ten_bak(:,:)	! back up of tensors
	type(path),allocatable::cluster(:,:)	! paths of clusters
	integer,allocatable::pos2clt(:,:,:)		! ori pos to clt pos
	logical,allocatable::to_calc(:,:)			! to calc tag
	logical,allocatable::backed_up(:,:)		! backed up tag

	contains
	private
	procedure,public:: initialize_nest
	procedure,public:: generate
	procedure,public:: set_cluster
	procedure,public:: set_tensor_nest
	procedure:: set_tensor_inner
	procedure,public:: get_outer_pos
	procedure,public:: calc_except
	procedure,public:: re_calc
	procedure,public:: restore_nest
	procedure,public:: check_exist_nest
	procedure,public:: in_clt
	procedure,public:: clt_pos
	procedure:: nest_name_inner
	procedure:: nest_name
end type

integer::test_lattice_clt=0

public nest_lattice, test_lattice_clt

contains

subroutine initialize_nest(LC,L)

	class(nest_lattice),intent(inout)::LC
	class(lattice),target,intent(inout)::L
	integer::L1,L2,i,j

	LC%lat_pre=>L
	call LC%lat_pre%get_size(L1,L2)
	allocate(LC%ten_res(L1,L2))
	allocate(LC%ten_bak(L1,L2))
	allocate(LC%cluster(L1,L2))
	do i=1,L1
		do j=1,L2
			call LC%cluster(i,j)%belong(LC%lat_pre)
		end do
	end do
	allocate(LC%to_calc(L1,L2))
	LC%to_calc=.false.
	allocate(LC%backed_up(L1,L2))
	LC%backed_up=.false.
	allocate(LC%pos2clt(L1,L2,2))
	LC%pos2clt=-1

end subroutine

subroutine generate(LC)

	class(nest_lattice),intent(inout)::LC
	integer::L1,L2,i,j,m,n

	call LC%lat_pre%get_size(L1,L2)
	call LC%lattice%initialize(LC%lat_pre%get_name()+'_nest',L1,L2,LC%lat_pre%get_max_nb_num())
	do i=1,L1
		do j=1,L2
			if (LC%cluster(i,j)%get_num()>0) then
				call calc_cluster(LC,[i,j])
				call LC%lattice%add([i,j],LC%lattice%get_name()+i+'_'+j,LC%ten_res(i,j))
				call LC%cluster(i,j)%set_name(LC%lattice%get_name()+i+'_'+j)
				!call LC%cluster(i,j)%draw('cl'+i+j)
			end if
		end do
	end do

	do i=1,L1
		do j=1,L2
			if (LC%lat_pre%check_exist([i,j]))then
				if (LC%pos2clt(i,j,1)<0)then
					call LC%lattice%add([i,j],LC%lat_pre,[i,j])
					call LC%lattice%set_contag([i,j],LC%lat_pre%get_contag([i,j]))
				end if
			end if
		end do
	end do
	!call LC%lattice%draw('lat')

	call LC%lattice%set_bond_as(LC%lat_pre,LC%cluster)
	!call LC%lattice%draw('lat')

end subroutine

subroutine set_cluster(LC,pos_clt,pos_site)

	class(nest_lattice),intent(inout)::LC
	integer,intent(in)::pos_clt(2),pos_site(2)

	call LC%cluster(pos_clt(1),pos_clt(2))%add(pos_site)
	LC%pos2clt(pos_site(1),pos_site(2),:)=pos_clt

end subroutine

subroutine calc_cluster(LC,pos)

	class(nest_lattice),intent(inout)::LC
	integer,intent(in)::pos(2)
	type(tn_tensor)::result

	call result%belong(LC%lat_pre)
	call result%absorb_all(LC%cluster(pos(1),pos(2)))
	LC%ten_res(pos(1),pos(2))=result

end subroutine

subroutine set_tensor_nest(LC,tenname,tenp,to_calc)

	class(nest_lattice),intent(inout)::LC
	character(len=*),intent(in)::tenname
	type(tensor),intent(inout)::tenp
	logical,intent(in)::to_calc
	character(len=max_char_length)::name_last,name_this

	if(LC%lattice%check_exist(tenname))then
		call LC%lattice%set_tensor(tenname,tenp) !A->B->C, when B->D, A should also ->D
	else
		call LC%set_tensor_inner(tenname,name_last,name_this,tenp,to_calc)
	end if

end subroutine

subroutine set_tensor_inner(LC,name,name_last,name_this,tenp,to_calc)

	class(nest_lattice),intent(inout)::LC
	character(len=*),intent(in)::name
	type(tensor),intent(inout)::tenp
	logical,intent(in)::to_calc
	character(len=*),intent(inout)::name_last,name_this ! name_last : cluster in lat_pre; name_this:  cluster in lattice
	integer::i,j,pos(2)

	if(LC%lat_pre%check_exist(name)) then
		name_last=name
		call LC%lat_pre%set_tensor(name_last,tenp)
	else
		select type(oldlat=>LC%lat_pre)
		type is (lattice)
			call wc_error_stop('lattice_clt.nest_name_inner','Site with name '//trim(name)//' not found in nesting search.')
		type is (nest_lattice)
			call oldlat%nest_name_inner(name,name_last,name_this)
			name_last=name_this
		end select
	end if
	if(LC%in_clt(name_last)) then
		pos=(LC%clt_pos(name_last))
		name_this=LC%cluster(pos(1),pos(2))%get_name()
	else
		name_this=name_last
	end if

	pos=LC%lat_pre%get_pos(name_last)
	i=LC%pos2clt(pos(1),pos(2),1)
	j=LC%pos2clt(pos(1),pos(2),2)
	LC%to_calc(i,j)=to_calc

end subroutine

subroutine re_calc(LC,back_up)

	class(nest_lattice),intent(inout)::LC
	logical,intent(in)::back_up
	integer::L1,L2,i,j

	select type(oldlat=>LC%lat_pre)
	type is (nest_lattice)
		call oldlat%re_calc(back_up)
	end select
	call LC%lat_pre%get_size(L1,L2)
	do i=1,L1
		do j=1,L2
			if (LC%to_calc(i,j))then
				if(back_up)then
					LC%backed_up(i,j)=.true.
					LC%ten_bak(i,j)=LC%ten_res(i,j)
				end if
				call calc_cluster(LC,[i,j])
			end if
		end do
	end do
	LC%to_calc=.false.

end subroutine

subroutine restore_nest(LC)

	class(nest_lattice),intent(inout)::LC
	integer::L1,L2,i,j

	select type(oldlat=>LC%lat_pre)
	type is (nest_lattice)
		call oldlat%restore_nest()
	end select
	call LC%lat_pre%get_size(L1,L2)
	do i=1,L1
		do j=1,L2
			if (LC%backed_up(i,j)) LC%ten_res(i,j)=LC%ten_bak(i,j)
		end do
	end do
	LC%backed_up=.false.

end subroutine

function get_outer_pos(LC,name) result(pos) ! pos in the outest lat

	class(nest_lattice),intent(inout)::LC
	character(len=*),intent(in)::name
	character(len=max_char_length)::name_last,name_this
	integer::pos(2)
	integer::L1,L2,i,j

	if (LC%lattice%check_exist(name)) then
		pos=LC%lattice%get_pos(name)
	else
		name_last=''
		name_this=''
		call LC%nest_name_inner(name,name_last,name_this)
		pos=LC%lattice%get_pos(name_this)
	end if	

end function

subroutine calc_except(LC,ten,name)

	class(nest_lattice),intent(inout)::LC
	type(tn_tensor),intent(inout)::ten
	character(len=*),intent(in)::name
	character(len=max_char_length)::name2
	integer::i,j,pos(2)

	if(.not. LC%lattice%check_exist(name)) then
		name2=LC%nest_name(name)  ! name2 in lat_pre contains name. if name not found in a nesting search report error
		pos=LC%clt_pos(name2)
		call ten%belong(LC%lat_pre)
		call ten%take_except(LC%cluster(pos(1),pos(2)))
		call ten%absorb_except(name2,LC%cluster(pos(1),pos(2)))
		if(name/=name2)then
			select type(oldlat=>LC%lat_pre)
			type is (nest_lattice)
				call oldlat%calc_except(ten,name)  ! lat_pre is a nesting lat and doesn't contains name
			end select
		end if		 
	end if

end subroutine

recursive function check_exist_nest(LC,name) result(res)

	class(nest_lattice),intent(inout)::LC
	character(len=*),intent(in)::name
	logical::res

	res=LC%lattice%check_exist(name)
	select type(oldlat=>LC%lat_pre)
	type is (lattice)
		res=res .or. oldlat%check_exist(name)
	type is (nest_lattice)
		res=res .or. oldlat%check_exist_nest(name)
	end select

end function

function in_clt(LC,name) result(res) ! not nesting

	class(nest_lattice),intent(inout)::LC
	character(len=*),intent(in)::name
	integer::pos(2)
	logical::res

	if(LC%lat_pre%check_exist(name)) then
		pos=LC%lat_pre%get_pos(name)
		res=(LC%pos2clt(pos(1),pos(2),1)>0)
	else
		call wc_error_stop('lattice_clt.in_clt','Site with name '//trim(name)//' not found.')
	end if

end function

function clt_pos(LC,name) result(res) ! not nesting

	class(nest_lattice),intent(inout)::LC
	character(len=*),intent(in)::name
	integer::res(2),pos(2)

	if(LC%lat_pre%check_exist(name)) then
		pos=LC%lat_pre%get_pos(name)
		res=LC%pos2clt(pos(1),pos(2),:)
	else
		call wc_error_stop('lattice_clt.clt_pos','Site with name '//trim(name)//' not found.')
	end if

end function

function nest_name(LC,name) result(res)

	class(nest_lattice),intent(inout)::LC
	character(len=*),intent(in)::name
	character(len=max_char_length)::res,name_this

	name_this=''
	res=''
	call LC%nest_name_inner(name,res,name_this)

end function

recursive subroutine nest_name_inner(LC,name,name_last,name_this)

	class(nest_lattice),intent(inout)::LC
	character(len=*),intent(in)::name
	character(len=*),intent(inout)::name_last,name_this ! name_last : cluster in lat_pre; name_this:  cluster in lattice
	integer::pos(2)

	if(LC%lat_pre%check_exist(name)) then
		name_last=name
	else
		select type(oldlat=>LC%lat_pre)
		type is (lattice)
			call wc_error_stop('lattice_clt.nest_name_inner','Site with name '//trim(name)//' not found in nesting search.')
		type is (nest_lattice)
			call oldlat%nest_name_inner(name,name_last,name_this)
			name_last=name_this
		end select
	end if
	if(LC%in_clt(name_last)) then
		pos=(LC%clt_pos(name_last))
		name_this=LC%cluster(pos(1),pos(2))%get_name()
	else
		name_this=name_last
	end if

end subroutine

end module