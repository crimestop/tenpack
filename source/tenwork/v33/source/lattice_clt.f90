module tensor_network_cluster
use tensor_network
use tensor_type
use error 
use tools
use string
use type_unidic
use mod_dictionary
use tn_tensor_type
implicit none

private

type lattice_clt
	private
	type(lattice),public::lat_pre
	type(lattice),public::lat
	type(lattice),allocatable::lat_clt(:,:)
	type(group),allocatable::cluster(:,:)
	integer,allocatable::pos2clt(:,:,:)		! ori pos to clt pos
	logical,allocatable::to_calc(:,:)
	logical,allocatable::backed_up(:,:)

	contains
	private
	procedure,public:: prepare
	procedure,public:: generate_cluster
	procedure,public:: set_cluster
	procedure,public:: set_tensor
	procedure,public:: get_outer_pos
	procedure,public:: calc_except
	procedure,public:: re_calc
	procedure,public:: restore
	procedure,public:: in_clt
	procedure,public:: clt_pos
	procedure,public:: get_names_clt
end type

integer::test_lattice_clt=0

public lattice_clt, test_lattice_clt

contains

subroutine prepare(LC)

	class(lattice_clt),intent(inout)::LC
	integer::L1,L2,i,j

	call LC%lat_pre%get_size(L1,L2)
	allocate(LC%lat_clt(L1,L2))
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

subroutine generate_cluster(LC)

	class(lattice_clt),intent(inout)::LC
	integer::L1,L2,i,j,m,n
	type(group)::tot

	call LC%lat_pre%get_size(L1,L2)
	call LC%lat%initialize(LC%lat_pre%get_name()+'_clt',L1,L2,LC%lat_pre%get_max_nb_num())
	call tot%belong(LC%lat_pre)
	do i=1,L1
		do j=1,L2
			if (LC%cluster(i,j)%get_num()>0) then
				call generate_cluster_single(LC%lat_pre,LC%lat_clt(i,j),LC%cluster(i,j),'lat_clt'+i+'_'+j)
				do m=1,L1
					do n=1,L2
						if(LC%cluster(i,j)%check_contain([m,n])) LC%pos2clt(m,n,:)=[i,j]
					end do
				end do
				call LC%lat%add([i,j],LC%lat_clt(i,j),[1,1])
				!call LC%lat%draw()
				call tot%take(LC%cluster(i,j))
			end if
		end do
	end do

	do i=1,L1
		do j=1,L2
			if (LC%lat_pre%check_exist([i,j]))then
				if (.not. tot%check_contain([i,j]))then
					call LC%lat%add([i,j],LC%lat_pre,[i,j])
				!call LC%lat%draw()
					call LC%lat%set_contag([i,j],LC%lat_pre%get_contag([i,j]))
				end if
			end if
		end do
	end do

	call LC%lat%set_bond_as(LC%lat_pre,LC%lat_clt)

end subroutine

subroutine set_cluster(LC,pos_clt,pos_site)

	class(lattice_clt),intent(inout)::LC
	integer,intent(in)::pos_clt(2),pos_site(2)

	call LC%cluster(pos_clt(1),pos_clt(2))%take(pos_site)

end subroutine

subroutine generate_cluster_single(lattice_ori,lat_cluster,cluster,name)
	! lat_cluster: lattice that contains the cluster
	! ten_cluster: tensor when cluster are contracted

	type(lattice),intent(in)::lattice_ori
	type(lattice),intent(inout)::lat_cluster
	type(group),intent(inout)::cluster
	character(len=*),intent(in)::name
	character(len=max_char_length), allocatable::names(:)
	type(tensor),pointer::tenp
	integer::num,nb_no,i,k
	character(len=max_char_length)::dir,nb_dir,nb_name

	names=cluster%get_names()
	num=size(names)
	call lat_cluster%initialize(name,1,num+1,num+1)
	call lat_cluster%add([1,1],name,save_tag=.true.)
	do i=1,num
		call lattice_ori%get_tensor_link(names(i),tenp)
		call lat_cluster%add([1,i+1],names(i),tenp)
		call lat_cluster%set_contag([1,i+1],lattice_ori%get_contag(names(i)))
		do k=1,lattice_ori%get_nb_num(names(i))
			call lattice_ori%get_bond_no(names(i),k,dir,nb_name,nb_no,nb_dir)
			if(.not.cluster%check_contain(nb_name))then
				call lat_cluster%set_bond(name,names(i),name+'.'+nb_name,dir)
			end if
		end do
	end do
	call lat_cluster%set_bond_as(lattice_ori)
	call calc_cluster(lat_cluster)

end subroutine

subroutine calc_cluster(lat_cluster)

	type(lattice),intent(inout)::lat_cluster
	type(tn_tensor)::result

	call result%belong(lat_cluster)
	call result%absorb_except([1,1])
	call result%invert_bond()
	call lat_cluster%update_tensor([1,1],result)

end subroutine

subroutine set_tensor(LC,tenname,tenp,to_calc)

	class(lattice_clt),intent(inout)::LC
	character(len=*),intent(in)::tenname
	type(tensor),intent(inout)::tenp
	logical,intent(in)::to_calc
	integer::L1,L2,i,j,pos(2),pos2(2)
	type(tn_tensor)::temp
	complex(8)::ws
	type(tensor),pointer::tp1,tp2

	call LC%lat_pre%get_size(L1,L2)
	call LC%lat_pre%set_tensor(tenname,tenp)
	if(LC%lat%check_exist(tenname))then
		call LC%lat%set_tensor(tenname,tenp) !A->B->C, when B->D, A should also ->D
	else
		pos=LC%lat_pre%get_pos(tenname)
		i=LC%pos2clt(pos(1),pos(2),1)
		j=LC%pos2clt(pos(1),pos(2),2)
		call LC%lat_clt(i,j)%set_tensor(tenname,tenp)
		LC%to_calc(i,j)=to_calc
	end if

end subroutine

subroutine re_calc(LC,back_up)

	class(lattice_clt),intent(inout)::LC
	logical,intent(in)::back_up
	integer::L1,L2,i,j
	type(tn_tensor)::temp
	complex(8)::ws

	call LC%lat_pre%get_size(L1,L2)
	do i=1,L1
		do j=1,L2
			if (LC%to_calc(i,j))then
				if(back_up)then
					LC%backed_up(i,j)=.true.
					call LC%lat_clt(i,j)%back_up([1,1])
				end if
				call calc_cluster(LC%lat_clt(i,j))
			end if
		end do
	end do
	LC%to_calc=.false.

end subroutine

subroutine restore(LC)

	class(lattice_clt),intent(inout)::LC
	integer::L1,L2,i,j
	type(tn_tensor)::temp
	complex(8)::ws

	call LC%lat_pre%get_size(L1,L2)
	do i=1,L1
		do j=1,L2
			if (LC%backed_up(i,j)) call LC%lat_clt(i,j)%restore([1,1])
		end do
	end do
	LC%backed_up=.false.

end subroutine

function get_outer_pos(LC,name) result(pos)

	class(lattice_clt),intent(inout)::LC
	character(len=*),intent(in)::name
	integer::pos(2)
	integer::L1,L2,i,j

	call LC%lat_pre%get_size(L1,L2)
	if (LC%lat%check_exist(name)) then
		pos=LC%lat%get_pos(name)
	else
		do i=1,L1
			do j=1,L2
				if (LC%cluster(i,j)%check_contain(name)) then
					pos=LC%lat%get_pos('lat_clt'+i+'_'+j)
				end if
			end do
		end do
	end if	

end function

subroutine calc_except(LC,ten,name)

	class(lattice_clt),intent(inout)::LC
	type(tn_tensor),intent(inout)::ten
	character(len=*),intent(in)::name
	integer::i,j,pos(2)

	if(.not. LC%lat%check_exist(name)) then
		pos=LC%lat_pre%get_pos(name)
		i=LC%pos2clt(pos(1),pos(2),1)
		j=LC%pos2clt(pos(1),pos(2),2)
		call ten%belong(LC%lat_clt(i,j))
		call ten%take([1,1])
		call ten%absorb_except(name)
		call ten%invert_bond()
	end if

end subroutine

function in_clt(LC,name) result(res)

	class(lattice_clt),intent(inout)::LC
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

function clt_pos(LC,name) result(res)

	class(lattice_clt),intent(inout)::LC
	character(len=*),intent(in)::name
	integer::res(2),pos(2)

	if(LC%lat_pre%check_exist(name)) then
		pos=LC%lat_pre%get_pos(name)
		res=LC%pos2clt(pos(1),pos(2),:)
	else
		call wc_error_stop('lattice_clt.clt_pos','Site with name '//trim(name)//' not found.')
	end if

end function

function get_names_clt(LC,pos) result(res)

	class(lattice_clt),intent(inout)::LC
	integer,intent(in)::pos(2)
	character(len=max_char_length),allocatable::res(:)

	res=LC%cluster(pos(1),pos(2))%get_names()

end function

end module