!
!                   _ooOoo_
!                  o8888888o
!                  88" . "88
!                  (| -_- |)
!                  O\  =  /O
!               ____/`---'\____
!             .'  \\|     |//  `.
!            /  \\|||  :  |||//  \
!           /  _||||| -:- |||||-  \
!           |   | \\\  -  /// |   |
!           | \_|  ''\---/''  |   |
!           \  .-\__  `-`  ___/-. /
!         ___`. .'  /--.--\  `. . __
!      ."" '<  `.___\_<|>_/___.'  >'"".
!     | | :  `- \`.;`\ _ /`;.`/ - ` : | |
!     \  \ `-.   \_ __\ /__ _/   .-` /  /
!======`-.____`-.___\_____/___.-`____.-'======
!                   `=---='
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!       Buddha blessed , no BUG 
! Report bugs of the package to sj.dong@outlook.com
module modify_module
	use usefull_function
	implicit none
	
contains



!******************************************* integer  ***************************
!int(i)=class(*)
	subroutine modifyTen_val_dim1_int(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		integer,intent(inout)::Tdata(LenT)
		class(*),intent(in)::value
		integer::icouter
		select type(value)
			type is (integer)
				Tdata(i)=value
			type is (real(kind=4))
				Tdata(i)=value
			type is (real(kind=8))
				Tdata(i)=value
			type is (complex(kind=4))
				Tdata(i)=value
			type is (complex(kind=8))
				Tdata(i)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine

	subroutine modifyTen_array_dim1_int(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		integer,intent(inout)::Tdata(LenT)
		class(*),intent(in)::value(LenV)
		integer::icouter
		select type(value)
			type is (integer)
				Tdata(i(1):i(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim2_int(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		integer,intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j)=value
			type is (real(kind=4))
				Tdata(i,j)=value
			type is (real(kind=8))
				Tdata(i,j)=value
			type is (complex(kind=4))
				Tdata(i,j)=value
			type is (complex(kind=8))
				Tdata(i,j)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim2_int(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		integer,intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value(LDV1,LDV2)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim3_int(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j,l)=value
			type is (real(kind=4))
				Tdata(i,j,l)=value
			type is (real(kind=8))
				Tdata(i,j,l)=value
			type is (complex(kind=4))
				Tdata(i,j,l)=value
			type is (complex(kind=8))
				Tdata(i,j,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim3_int(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value(LDV1,LDV2,LDV3)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim4_int(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j,k,l)=value
			type is (real(kind=4))
				Tdata(i,j,k,l)=value
			type is (real(kind=8))
				Tdata(i,j,k,l)=value
			type is (complex(kind=4))
				Tdata(i,j,k,l)=value
			type is (complex(kind=8))
				Tdata(i,j,k,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim4_int(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine


!******************************************* real*4  ***************************
!real*4(i)=class(*)
	subroutine modifyTen_val_dim1_real4(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=4),intent(inout)::Tdata(LenT)
		class(*),intent(in)::value
		integer::icouter
		select type(value)
			type is (integer)
				Tdata(i)=value
			type is (real(kind=4))
				Tdata(i)=value
			type is (real(kind=8))
				Tdata(i)=value
			type is (complex(kind=4))
				Tdata(i)=value
			type is (complex(kind=8))
				Tdata(i)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine

	subroutine modifyTen_array_dim1_real4(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=4),intent(inout)::Tdata(LenT)
		class(*),intent(in)::value(LenV)
		integer::icouter
		select type(value)
			type is (integer)
				Tdata(i(1):i(2))=value(1:LenV)
			type is (real(kind=4))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (real(kind=8))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (complex(kind=4))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (complex(kind=8))
				Tdata(i(1):i(2))=value(1:LenV)
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim2_real4(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j)=value
			type is (real(kind=4))
				Tdata(i,j)=value
			type is (real(kind=8))
				Tdata(i,j)=value
			type is (complex(kind=4))
				Tdata(i,j)=value
			type is (complex(kind=8))
				Tdata(i,j)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim2_real4(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value(LDV1,LDV2)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim3_real4(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j,l)=value
			type is (real(kind=4))
				Tdata(i,j,l)=value
			type is (real(kind=8))
				Tdata(i,j,l)=value
			type is (complex(kind=4))
				Tdata(i,j,l)=value
			type is (complex(kind=8))
				Tdata(i,j,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim3_real4(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value(LDV1,LDV2,LDV3)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim4_real4(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j,k,l)=value
			type is (real(kind=4))
				Tdata(i,j,k,l)=value
			type is (real(kind=8))
				Tdata(i,j,k,l)=value
			type is (complex(kind=4))
				Tdata(i,j,k,l)=value
			type is (complex(kind=8))
				Tdata(i,j,k,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim4_real4(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine


!******************************************* real*8  ***************************
!real*8(i)=class(*)

	subroutine modifyTen_val_dim1_real8(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=8),intent(inout)::Tdata(LenT)
		class(*),intent(in)::value
		integer::icouter
		select type(value)
			type is (integer)
				Tdata(i)=value
			type is (real(kind=4))
				Tdata(i)=value
			type is (real(kind=8))
				Tdata(i)=value
			type is (complex(kind=4))
				Tdata(i)=value
			type is (complex(kind=8))
				Tdata(i)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim1_real8(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=8),intent(inout)::Tdata(LenT)
		class(*),intent(in)::value(LenT)
		integer::icouter
		select type(value)
			type is (integer)
				Tdata(i(1):i(2))=value(1:LenV)
			type is (real(kind=4))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (real(kind=8))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (complex(kind=4))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (complex(kind=8))
				Tdata(i(1):i(2))=value(1:LenV)
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim2_real8(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j)=value
			type is (real(kind=4))
				Tdata(i,j)=value
			type is (real(kind=8))
				Tdata(i,j)=value
			type is (complex(kind=4))
				Tdata(i,j)=value
			type is (complex(kind=8))
				Tdata(i,j)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim2_real8(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value(LDV1,LDV2)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim3_real8(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j,l)=value
			type is (real(kind=4))
				Tdata(i,j,l)=value
			type is (real(kind=8))
				Tdata(i,j,l)=value
			type is (complex(kind=4))
				Tdata(i,j,l)=value
			type is (complex(kind=8))
				Tdata(i,j,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim3_real8(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value(LDV1,LDV2,LDV3)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim4_real8(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j,k,l)=value
			type is (real(kind=4))
				Tdata(i,j,k,l)=value
			type is (real(kind=8))
				Tdata(i,j,k,l)=value
			type is (complex(kind=4))
				Tdata(i,j,k,l)=value
			type is (complex(kind=8))
				Tdata(i,j,k,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim4_real8(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine



!******************************************* complex(kind=4)  ***************************
!complex(kind=4) (i)=class(*)
	subroutine modifyTen_val_dim1_com4(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=4),intent(inout)::Tdata(LenT)
		class(*),intent(in)::value
		integer::icouter
		select type(value)
			type is (integer)
				Tdata(i)=value
			type is (real(kind=4))
				Tdata(i)=value
			type is (real(kind=8))
				Tdata(i)=value
			type is (complex(kind=4))
				Tdata(i)=value
			type is (complex(kind=8))
				Tdata(i)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine

	subroutine modifyTen_array_dim1_com4(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=4),intent(inout)::Tdata(LenT)
		class(*),intent(in)::value(LenT)
		integer::icouter
		select type(value)
			type is (integer)
				Tdata(i(1):i(2))=value(1:LenV)
			type is (real(kind=4))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (real(kind=8))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (complex(kind=4))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (complex(kind=8))
				Tdata(i(1):i(2))=value(1:LenV)
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim2_com4(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j)=value
			type is (real(kind=4))
				Tdata(i,j)=value
			type is (real(kind=8))
				Tdata(i,j)=value
			type is (complex(kind=4))
				Tdata(i,j)=value
			type is (complex(kind=8))
				Tdata(i,j)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim2_com4(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value(LDV1,LDV2)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim3_com4(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j,l)=value
			type is (real(kind=4))
				Tdata(i,j,l)=value
			type is (real(kind=8))
				Tdata(i,j,l)=value
			type is (complex(kind=4))
				Tdata(i,j,l)=value
			type is (complex(kind=8))
				Tdata(i,j,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim3_com4(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value(LDV1,LDV2,LDV3)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim4_com4(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j,k,l)=value
			type is (real(kind=4))
				Tdata(i,j,k,l)=value
			type is (real(kind=8))
				Tdata(i,j,k,l)=value
			type is (complex(kind=4))
				Tdata(i,j,k,l)=value
			type is (complex(kind=8))
				Tdata(i,j,k,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim4_com4(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine






!******************************************* complex(kind=8)  ***************************
!complex(kind=8)(i)=class(*)
	subroutine modifyTen_val_dim1_com8(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=8),intent(inout)::Tdata(LenT)
		class(*),intent(in)::value
		integer::icouter
		select type(value)
			type is (integer)
				Tdata(i)=value
			type is (real(kind=4))
				Tdata(i)=value
			type is (real(kind=8))
				Tdata(i)=value
			type is (complex(kind=4))
				Tdata(i)=value
			type is (complex(kind=8))
				Tdata(i)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine

	subroutine modifyTen_array_dim1_com8(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=8),intent(inout)::Tdata(LenT)
		class(*),intent(in)::value(LenT)
		integer::icouter
		select type(value)
			type is (integer)
				Tdata(i(1):i(2))=value(1:LenV)
			type is (real(kind=4))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (real(kind=8))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (complex(kind=4))
				Tdata(i(1):i(2))=value(1:LenV)
			type is (complex(kind=8))
				Tdata(i(1):i(2))=value(1:LenV)
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim2_com8(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j)=value
			type is (real(kind=4))
				Tdata(i,j)=value
			type is (real(kind=8))
				Tdata(i,j)=value
			type is (complex(kind=4))
				Tdata(i,j)=value
			type is (complex(kind=8))
				Tdata(i,j)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim2_com8(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value(LDV1,LDV2)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim3_com8(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j,l)=value
			type is (real(kind=4))
				Tdata(i,j,l)=value
			type is (real(kind=8))
				Tdata(i,j,l)=value
			type is (complex(kind=4))
				Tdata(i,j,l)=value
			type is (complex(kind=8))
				Tdata(i,j,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim3_com8(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value(LDV1,LDV2,LDV3)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim4_com8(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value
		select type(value)
			type is (integer)
				Tdata(i,j,k,l)=value
			type is (real(kind=4))
				Tdata(i,j,k,l)=value
			type is (real(kind=8))
				Tdata(i,j,k,l)=value
			type is (complex(kind=4))
				Tdata(i,j,k,l)=value
			type is (complex(kind=8))
				Tdata(i,j,k,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim4_com8(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		select type(value)
			type is (integer)
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (real(kind=4))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (real(kind=8))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (complex(kind=4))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			type is (complex(kind=8))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine


!******************************************* logical  ***************************
!logical(i)=class(*)
	subroutine modifyTen_val_dim1_logi(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		logical,intent(inout)::Tdata(LenT)
		class(*),intent(in)::value
		integer::icouter
		select type(value)
			type is (logical)
				Tdata(i)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine

	subroutine modifyTen_array_dim1_logi(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		logical,intent(inout)::Tdata(LenT)
		class(*),intent(in)::value(LenT)
		integer::icouter
		select type(value)
			type is (logical)
				Tdata(i(1):i(2))=value(1:LenV)
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim2_logi(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		logical,intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value
		select type(value)
			type is (logical)
				Tdata(i,j)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim2_logi(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		logical,intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value(LDV1,LDV2)
		select type(value)
			type is (logical)
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim3_logi(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value
		select type(value)
			type is (logical)
				Tdata(i,j,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim3_logi(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value(LDV1,LDV2,LDV3)
		select type(value)
			type is (logical)
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_val_dim4_logi(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value
		select type(value)
			type is (logical)
				Tdata(i,j,k,l)=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine modifyTen_array_dim4_logi(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		select type(value)
			type is (logical)
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine

!******************************************* character  ***************************
!character(i)=character(len=len_of_char_in_usefull_function)
	subroutine modifyTen_val_dim1_char(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		character(len=*),intent(inout)::Tdata(LenT)
		class(*),intent(in)::value
		select type(value)
			type is (character(len=*))
				Tdata(i)=value
			type is (integer)
				Tdata(i)=value
			type is (real(kind=4))
				Tdata(i)=value
			type is (real(kind=8))
				Tdata(i)=value
			type is (complex(kind=4))
				Tdata(i)=value
			type is (complex(kind=8))
				Tdata(i)=value
			type is (logical)
				Tdata(i)=value
		end select
		return
	end subroutine

	subroutine modifyTen_array_dim1_char(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		character(len=*),intent(inout)::Tdata(LenT)
		class(*),intent(in)::value(LenT)
		select type(value)
			type is (character(len=*))
				Tdata(i(1):i(2))=value(1:LenV)
		end select
		return
	end subroutine
	subroutine modifyTen_val_dim2_char(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value
		select type(value)
			type is (character(len=*))
					Tdata(i,j)=value
			type is (integer)
				Tdata(i,j)=value
			type is (real(kind=4))
				Tdata(i,j)=value
			type is (real(kind=8))
				Tdata(i,j)=value
			type is (complex(kind=4))
				Tdata(i,j)=value
			type is (complex(kind=8))
				Tdata(i,j)=value
			type is (logical)
				Tdata(i,j)=value
		end select
		return
	end subroutine
	subroutine modifyTen_array_dim2_char(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		class(*),intent(in)::value(LDV1,LDV2)
		select type(value)
			type is (character(len=*))
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		end select
		return
	end subroutine
	subroutine modifyTen_val_dim3_char(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value
		select type(value)
			type is (character(len=*))
					Tdata(i,j,l)=value
			type is (integer)
				Tdata(i,j,l)=value
			type is (real(kind=4))
				Tdata(i,j,l)=value
			type is (real(kind=8))
				Tdata(i,j,l)=value
			type is (complex(kind=4))
				Tdata(i,j,l)=value
			type is (complex(kind=8))
				Tdata(i,j,l)=value
			type is (logical)
				Tdata(i,j,l)=value
		end select
		return
	end subroutine
	subroutine modifyTen_array_dim3_char(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		class(*),intent(in)::value(LDV1,LDV2,LDV3)
		select type(value)
			type is (character(len=*))
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		end select
		return
	end subroutine
	subroutine modifyTen_val_dim4_char(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value
		select type(value)
			type is (character(len=*))
					Tdata(i,j,k,l)=value
			type is (integer)
				Tdata(i,j,k,l)=value
			type is (real(kind=4))
				Tdata(i,j,k,l)=value
			type is (real(kind=8))
				Tdata(i,j,k,l)=value
			type is (complex(kind=4))
				Tdata(i,j,k,l)=value
			type is (complex(kind=8))
				Tdata(i,j,k,l)=value
			type is (logical)
				Tdata(i,j,k,l)=value
		end select
		return
	end subroutine
	subroutine modifyTen_array_dim4_char(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		class(*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		select type(value)
			type is (character(len=*))
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		end select
		return
	end subroutine


!****************************************
!! call T%setValue((/1,2/),'a') will not set 'a'm as when class(*) is chatacer,the length of which is 0,that is class(*) is character(len=0)
! character(len=10)::char
! char='a'
! call T%setValue((/1,2/),char) these 3 code will go right
!******************************************* character  ***************************
!character(i)=character(len=len_of_char_in_usefull_function)
	subroutine modifyTen_val_dim1_character(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		character(len=*),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_array_dim1_character(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		character(len=*),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value(LenT)
				Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine
	subroutine modifyTen_val_dim2_character(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value
				Tdata(i,j)=value
		return
	end subroutine
	subroutine modifyTen_array_dim2_character(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value(LDV1,LDV2)
				Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine
	subroutine modifyTen_val_dim3_character(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value
				Tdata(i,j,l)=value
		return
	end subroutine
	subroutine modifyTen_array_dim3_character(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3)
				Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine
	subroutine modifyTen_val_dim4_character(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value
				Tdata(i,j,k,l)=value
		return
	end subroutine
	subroutine modifyTen_array_dim4_character(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
				Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine













!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!
!        A(i1:i2)=B(i1:i2)
!***********************************************************************************************************


	subroutine store_value_int(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		integer,intent(inout)::A(LDA)
		class(*),intent(in)::B(LDB)
		select type(B)
			type is (integer)
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (real(kind=4))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (real(kind=8))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (complex(kind=4))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (complex(kind=8))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value_real4(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real*4,intent(inout)::A(LDA)
		class(*),intent(in)::B(LDB)
		select type(B)
			type is (integer)
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (real(kind=4))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (real(kind=8))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (complex(kind=4))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (complex(kind=8))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value_real8(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real*8,intent(inout)::A(LDA)
		class(*),intent(in)::B(LDB)
		select type(B)
			type is (integer)
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (real(kind=4))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (real(kind=8))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (complex(kind=4))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (complex(kind=8))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value_com4(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex*8,intent(inout)::A(LDA)
		class(*),intent(in)::B(LDB)
		select type(B)
			type is (integer)
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (real(kind=4))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (real(kind=8))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (complex(kind=4))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (complex(kind=8))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value_com8(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex*16,intent(inout)::A(LDA)
		class(*),intent(in)::B(LDB)
		select type(B)
			type is (integer)
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (real(kind=4))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (real(kind=8))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (complex(kind=4))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			type is (complex(kind=8))
				A(ia(1):ia(2))=B(ib(1):ib(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value_logi(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		logical,intent(inout)::A(LDA)
		class(*),intent(in)::B(LDB)
		select type(B)
			type is (logical)
				A(ia(1):ia(2))=B(ib(1):ib(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value_char(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		character(len=*),intent(inout)::A(LDA)
		character(len=*),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!
!        A(i1:i2,j1:j2)=B(i1:i2,j1:j2)
!
!***********************************************************************************************************

	subroutine store_value2_int_int(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_int_real4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_int_real8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_int_com4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_int_com8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_int(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		class(*),intent(in)::B(LDB,LDB2)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	
	
	
	
	
	
	subroutine store_value2_real4_int(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*4,intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_real4_real4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*4,intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_real4_real8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*4,intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_real4_com4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*4,intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_real4_com8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*4,intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_real4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*4,intent(inout)::A(LDA,LDA2)
		class(*),intent(in)::B(LDB,LDB2)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	
	
	
	
	
	
	
	
	
	
	subroutine store_value2_real8_int(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*8,intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_real8_real4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*8,intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_real8_real8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*8,intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_real8_com4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*8,intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_real8_com8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*8,intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_real8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real*8,intent(inout)::A(LDA,LDA2)
		class(*),intent(in)::B(LDB,LDB2)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	
	
	subroutine store_value2_com4_int(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*8,intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_com4_real4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*8,intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_com4_real8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*8,intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_com4_com4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*8,intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_com4_com8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*8,intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_com4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*8,intent(inout)::A(LDA,LDA2)
		class(*),intent(in)::B(LDB,LDB2)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	
	
	
	
	
	
	
	
	subroutine store_value2_com8_int(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*16,intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_com8_real4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*16,intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_com8_real8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*16,intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_com8_com4(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*16,intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_com8_com8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*16,intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	subroutine store_value2_com8(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex*16,intent(inout)::A(LDA,LDA2)
		class(*),intent(in)::B(LDB,LDB2)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	
	
	subroutine store_value2_logi_logi(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		logical,intent(inout)::A(LDA,LDA2)
		logical,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine
	
	subroutine store_value2_logi(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		logical,intent(inout)::A(LDA,LDA2)
		class(*),intent(in)::B(LDB,LDB2)
		select type(B)
			type is (logical)
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value2_char(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		character(len=*),intent(inout)::A(LDA,LDA2)
		character(len=*),intent(in)::B(LDB,LDB2)
				A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
	end subroutine	


!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!
!        A(i1:i2,j1:j2,k1:k2)=B(i1:i2,j1:j2,k1:k2)
!
!***********************************************************************************************************
	
	subroutine store_value3_int(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3)
		class(*),intent(in)::B(LDB,LDB2,LDB3)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value3_real4(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real*4,intent(inout)::A(LDA,LDA2,LDA3)
		class(*),intent(in)::B(LDB,LDB2,LDB3)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value3_real8(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real*8,intent(inout)::A(LDA,LDA2,LDA3)
		class(*),intent(in)::B(LDB,LDB2,LDB3)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value3_com4(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex*8,intent(inout)::A(LDA,LDA2,LDA3)
		class(*),intent(in)::B(LDB,LDB2,LDB3)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value3_com8(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex*16,intent(inout)::A(LDA,LDA2,LDA3)
		class(*),intent(in)::B(LDB,LDB2,LDB3)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value3_logi(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3)
		class(*),intent(in)::B(LDB,LDB2,LDB3)
		select type(B)
			type is (logical)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine store_value3_char(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine
!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!***********************************************************************************************************
!
!        A(i1:i2,j1:j2,k1:k2,l1:l2)=B(i1:i2,j1:j2,k1:k2,l1:l2)
!
!***********************************************************************************************************
	
	subroutine store_value4_int(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		class(*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine	
	subroutine store_value4_real4(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real*4,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		class(*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine	
	subroutine store_value4_real8(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real*8,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		class(*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine	
	subroutine store_value4_com4(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex*8,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		class(*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine	
	subroutine store_value4_com8(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex*16,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		class(*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		select type(B)
			type is (integer)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (real(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (real(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (complex(kind=4))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			type is (complex(kind=8))
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine	
	subroutine store_value4_logi(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		class(*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		select type(B)
			type is (logical)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
			class default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine	
	subroutine store_value4_char(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
				A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	





end 
