program md
	use verlet
	use vector

	implicit real(8) (a-h,o-z)

	! Time step in units of 1.018E-14 sec, mass of silicon, boltzmann constant
	parameter(dt=0.10d0,rmass=28.d0,rkboltz=8.6173855d-5)
	! rxyz are the atomic positions, vxyz the velocities and fxyz and gxyz the 
	! current and previous forces needed for the velocity verlet algorithm
	real(8), allocatable, dimension(:,:) :: rxyz,vxyz,fxyz,gxyz
	real(8), allocatable, dimension(:) :: m;
	! alat is the size of the simulation cell
	real(8) :: alat(3)
	character(27) filename
	character(3) fn
	procedure(force_func), pointer :: F;
	integer :: x;

	open(unit=2,file='./OUTPUT_COLD/MDiter',status='unknown')

	open(unit=1,file='./INPUT/cold.dat',status='old')
	! read alat,nat
	read(1,*) alat(1),alat(2),alat(3)
	read(1,*) nat
	write(*,'(a,x,i7,3(x,e12.5))') 'nat,alat',nat,alat

	! allocate arrays
	allocate(rxyz(3,nat),vxyz(3,nat),fxyz(3,nat),gxyz(3,nat),m(nat))
	m = rmass
	F => force;

	! read positions velocities
	do iat=1,nat
		    read(1,*) (rxyz(j,iat),j=1,3),(vxyz(j,iat),j=1,3)
	enddo
	close(1)

	! first forces: gxyz
	call bazant(nat,alat,rxyz,gxyz,pener,coord,ener_var,coord_var,count)

	nstep=2*10**5
	do 1000,istep=0,nstep

	!      Evolution of the system according to 'VELOCITY VERLET' algorithm
	! ADD THIS PART TO THE PROGRAM
	call verlet_force(nat, rxyz, vxyz, F, m, dt);

	!................

	! further forces: fxyz
	!call bazant(nat,alat,rxyz,fxyz,pener,coord,ener_var,coord_var,count)

	!................

	! instantaneous temperature
	abs_sum=0;
	do x=1,nat
		abs_sum = abs_sum + norm_3d(vxyz(:,x))**2.d0;
	enddo
	temp = abs_sum*rmass/((3*nat-3)*rkboltz);
	rkin = abs_sum*rmass/2.d0;
	!................
	!
	! Lets denote by pener the potential energy, by rkin the kinetic energy 
	! and by energy the total energy
	energy= pener+rkin
	if (istep.eq.0) energy0=energy
	energy= energy-energy0
	! write energies and temperature every 100 timesteps 
	if (mod(istep,100).eq.0)   & 
	 write(2,'(i9,x,e17.10,2(x,e12.5),x,e11.4)')  istep,energy,pener,rkin,temp

	! write the current atomic position after every mplot timesteps 
	! into a visualization file
	mplot=nstep/100!max(nstep/100,100)
	if (mod(istep,mplot).eq.0) then 
			mm=istep/mplot

			!  generate filename and open files
			if (mm.lt.10) then
			        write(fn,'(i1)') mm
			        fn='00'//fn
			else if (mm.lt.100) then
			        write(fn,'(i2)') mm
			        fn='0'//fn
			else if (mm.lt.1000) then
			        write(fn,'(i3)') mm
			else
			        write(*,*) 'more than 1000 files requested'
			        goto 1000
			endif
			filename = './OUTPUT_COLD/'//'pos'//fn//'.ascii';
			write(6,*) fn,"%";
			open(unit=1,file=filename,status='unknown')

			write(1,*) '        '
			write(1,'(3(x,e14.7))') alat(1),0.d0,alat(2)
			write(1,'(3(x,e14.7))') 0.d0,0.d0,alat(3)
			do iat=1,nat
			        write(1,'(3(x,e14.7),a)') (rxyz(j,iat),j=1,3),'  Si'
			enddo
			close(1)
	endif

	1000    continue

	deallocate(rxyz,vxyz,fxyz,gxyz)
	close(2)

	contains
	function force(n, pos)
		real(8), dimension(3,n) :: force;
		integer, intent(in) :: n;
		real(8), dimension(3,n), intent(in) :: pos;
		call bazant(n,alat,pos,fxyz,pener,coord,ener_var,coord_var,count)
		force = fxyz;
	end function force

end
