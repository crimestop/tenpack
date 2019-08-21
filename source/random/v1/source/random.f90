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
		integer::seed=0
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

	end type

public randomer

contains

logical function check_uninited(myrand)

	class(randomer),intent(inout)::myrand

	check_uninited=myrand%first_tag

end function

subroutine clean(myrand)

	class(randomer),intent(inout)::myrand

	myrand%seed=0
	myrand%shuffle(ntab)=0
	myrand%first_tag=.true.

end subroutine

subroutine initialize1(myrand,myseed,nproc,my_rank,subseed_)  ! all procs has different random number

	class(randomer),intent(inout)::myrand
	integer,intent(in)::nproc,my_rank
	integer,intent(inout)::myseed
	integer,intent(out),optional::subseed_
	integer::counter
	integer::subseed,i
	type(randomer)::seeder

	if(myseed==0)then
		call system_clock(counter)
		myseed=-modulo(counter,1000000)-1
	end if
	call set_seed_in(seeder,myseed)

	do i=0,my_rank
		subseed= int(-5970*seeder%randreal())
	end do
	call set_seed_in(myrand,subseed)

	if(present(subseed_))subseed_=subseed

end subroutine

subroutine initialize2(myrand,myseed)  ! all procs has same random number

	class(randomer),intent(inout)::myrand
	integer,intent(inout)::myseed
	integer(8)::counter

	if(myseed==0)then
		call system_clock(counter)
		myseed=-mod(int(counter),1000000)-1
	end if
	call set_seed_in(myrand,myseed)

end subroutine

subroutine set_seed_in(myrand,seed)

	class(randomer),intent(inout)::myrand
	integer,intent(in)::seed
	integer::j

	if(.not. myrand%first_tag)then
		call wc_error_stop('random-set_seed_in','random number generator has already been initialized')
	end if
	myrand%seed = max(-seed,1)
	do j = ntab+8, 1, -1
		myrand%seed = ia*mod(myrand%seed,iq)-ir*(myrand%seed/iq)
		if (myrand%seed < 0) myrand%seed = myrand%seed + im
		if (j <= ntab) myrand%shuffle(j) = myrand%seed
	enddo
	myrand%first_tag=.false.
	myrand%rand=myrand%shuffle(1)

end subroutine

real*8 function randreal(myrand)

	class(randomer),intent(inout)::myrand
	integer::pos

	if(myrand%first_tag)then
		call wc_error_stop('random-rand','random number generator has been used before initialized')
	end if
	myrand%seed = ia*mod(myrand%seed,iq) - ir*(myrand%seed/iq)
	if (myrand%seed<0) myrand%seed = myrand%seed + im
	pos = 1 + myrand%rand/ndiv
	myrand%rand = myrand%shuffle(pos)
	myrand%shuffle(pos) = myrand%seed
	randreal = min(am*myrand%rand,rnmx)

end function

integer function randInteger(myrand,iMin, iMax)

	class(randomer),intent(inout)::myrand
	integer, intent(in)::iMin, iMax

	if (iMin > iMax) then
		call wc_error_stop('random-randInteger','iMin should be smaller than iMax!')
	end if
	randInteger = floor(iMin + myrand%randreal() * (iMax - iMin + 1))

end function 

end module