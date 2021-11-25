module harmonic_oszilator
		use vector;
        implicit none;
        
        real(8) :: k = 0.5d0;
        
contains
        function harmonic_force(N, pos)
                real(8), dimension(3,N) :: harmonic_force;
                integer, intent(in) :: N;
                real(8), dimension(3,N), intent(in) :: pos;
                harmonic_force = -k*pos;
        end function harmonic_force
        
        function harmonic_potential(N, pos)
                real(8) :: harmonic_potential;
                integer, intent(in) :: N;
                real(8), dimension(3,N), intent(in) :: pos;
                integer :: x;
                harmonic_potential = 0.d0;
                do x = 1,N
                        harmonic_potential = harmonic_potential + (k/2.d0)*(norm_3d(pos(:,x))**2.d0);
                enddo
        end function harmonic_potential
        
        function kinetic_energy(N, vel, m)
                real(8) :: kinetic_energy;
                integer, intent(in) :: N;
                real(8), dimension(3,N), intent(in) :: vel;
                real(8), dimension(N), intent(in) :: m;
                integer :: x;
                kinetic_energy = 0.d0;
                do x = 1,N
                        kinetic_energy = kinetic_energy + (m(x)/2.d0) * norm_3d(vel(:,x))**2.d0;
                enddo
        end function kinetic_energy
        
end module harmonic_oszilator
