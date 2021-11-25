program verlet_test
        use verlet;
        use harmonic_oszilator;
        use vector;
        use differentiation;
        implicit none;
        
        integer :: x, N = 2, iter = 10000;
        real(8), dimension(3,2) :: pos, vel;
		procedure (grad_func), pointer :: g;
        real(8), dimension(2) :: m = (/1.d0, 2.3d0/);
        real(8) :: e_kin, e_pot, h, t = 0, t_max = 10;
        procedure (force_func), pointer :: F => harmonic_force;
        pos(1,1) = 1.d0; pos(2,1) = 0.d0; pos(3,1) = 3.d0;
        vel(1,1) = 0.d0; vel(2,1) = 10.d0; vel(3,1) = 0.d0;
        pos(1,2) = -1.d0; pos(2,2) = 0.d0; pos(3,2) = 4.d0;
        vel(1,2) = 0.d0; vel(2,2) = 0.1d0; vel(3,2) = 0.d0;
        h = t_max/iter;
        k = 5.d0;
        open(1,FILE='./OUTPUT/energy');
        open(2,FILE='./OUTPUT/pos_1');
        open(3,FILE='./OUTPUT/pos_2');
        do x = 1,iter+1
                e_kin = kinetic_energy(N,vel,m);
                e_pot = harmonic_potential(N,pos);
                write(1,*) t, e_kin, e_pot, e_kin + e_pot;
                write(2,*) t, pos(:,1);
                write(3,*) t, pos(:,2);
                call verlet_force(N,pos,vel,F,m,h);
                t = t + h;
        enddo
        close(1);
        close(2);
        close(3);
end program


