module mod_rand
use error
implicit none
private

	integer,parameter::ia=16807
	integer,parameter::im=2147483647
	integer,parameter::iq=127773
	integer,parameter::ir=2836
	integer,parameter::ntab=32
	integer,parameter::ndiv=1+(im-1)/ntab
	real(8),parameter::am=1./im
	real(8),parameter::rnmx=1.0-1.2e-7

	type randomer
		private
		integer::seed
		integer::this_seed
		integer::state
		integer::rand
		integer::shuffle(ntab)=0
		logical::first_tag=.true.
	contains
		private
		procedure,public::check_uninited
		procedure,public::clean
		procedure::initialize1
		procedure::initialize2
		generic,public::initialize=>initialize1,initialize2
		procedure,public::randreal
		procedure,public::randInteger
		procedure,public::get_seed
		procedure,public::get_subseed

	end type

public randomer

contains

logical function check_uninited(myrand)

	class(randomer),intent(inout)::myrand

	check_uninited=myrand%first_tag

end function

subroutine clean(myrand)

	class(randomer),intent(inout)::myrand

	myrand%first_tag=.true.

end subroutine

subroutine initialize1(myrand,myseed,my_rank)  ! all procs has different random number

	class(randomer),intent(inout)::myrand
	integer,intent(in)::my_rank
	integer,intent(in)::myseed
	integer::counter
	integer::seed,subseed,i
	type(randomer)::seeder

	if(.not. myrand%first_tag)then
		call wc_error_stop('random.initialize','random number generator has already been initialized')
		return
	end if

	if(myseed==0)then
		call system_clock(counter)
		seed=modulo(counter,1000000)+1
	else
		if(myseed<0 .or. myseed>1000000)then
			call wc_error_stop('random.initialize','seed out of range')
			return
		end if
		seed=myseed
	end if
	call initialize2(seeder,seed)
	do i=0,my_rank
		subseed=seeder%randInteger(1,1000000)
	end do
	myrand%seed=seed
	myrand%this_seed=subseed
	call prepare(myrand)
	myrand%first_tag=.false.

end subroutine

subroutine initialize2(myrand,myseed)  ! all procs has same random number

	class(randomer),intent(inout)::myrand
	integer,intent(in)::myseed
	integer(8)::counter
	integer::seed

	if(.not. myrand%first_tag)then
		call wc_error_stop('random.initialize','random number generator has already been initialized')
		return
	end if

	if(myseed==0)then
		call system_clock(counter)
		seed=modulo(counter,1000000)+1
	else
		if(myseed<0 .or. myseed>1000000)then
			call wc_error_stop('random.initialize','seed out of range')
			return
		end if
		seed=myseed
	end if
	myrand%seed=seed
	myrand%this_seed=seed
	call prepare(myrand)
	myrand%first_tag=.false.

end subroutine

subroutine prepare(myrand)			! generate shuffle

	class(randomer),intent(inout)::myrand
	integer::j

	myrand%state = myrand%this_seed
	do j = ntab+8, 1, -1 ! ignore first 8 rands
		call next_state(myrand%state)
		if (j <= ntab) myrand%shuffle(j) = myrand%state
	enddo
	myrand%rand=myrand%shuffle(1)

end subroutine

subroutine next_state(state)

	integer,intent(inout)::state

	state = ia*mod(state,iq) - ir*(state/iq)
	if (state<0) state = state + im

	! state = q * x + y
	! new state = a * y - r * x

end subroutine

real(8) function randreal(myrand)

	class(randomer),intent(inout)::myrand
	integer::pos

	if(myrand%first_tag)then
		call wc_error_stop('random.randreal','random number generator has been used before initialized')
		randreal=0d0
		return
	end if
	call next_state(myrand%state)
	pos = 1 + myrand%rand/ndiv
	myrand%rand = myrand%shuffle(pos)
	myrand%shuffle(pos) = myrand%state
	randreal = min(am*myrand%rand,rnmx)

	! pos = last rand in 1 - div
	! rand is shuffle(pos)
	! shuffle(pos) is state

end function

integer function randInteger(myrand,iMin, iMax)

	class(randomer),intent(inout)::myrand
	integer, intent(in)::iMin, iMax

	if (iMin > iMax) then
		call wc_error_stop('random.randInteger','iMin should be smaller than iMax!')
		randInteger=0
		return
	end if
	randInteger = floor(iMin + myrand%randreal() * (iMax - iMin + 1))

end function 

integer function get_seed(myrand)

	class(randomer),intent(inout)::myrand

	get_seed=myrand%seed

end function 

integer function get_subseed(myrand)

	class(randomer),intent(inout)::myrand

	get_subseed=myrand%this_seed

end function 

end module