module verlet
        implicit none;
        
        interface
                function force_func(N, pos)
                        real(8), dimension(3,N) :: force_func;
                        integer, intent(in) :: N;
                        real(8), dimension(3,N), intent(in) :: pos;
                end function force_func
                
                function potential_func(N, pos)
                        real(8) :: potential_func;
                        integer, intent(in) :: N;
                        real(8), dimension(3,N), intent(in) :: pos;
                end function potential_func
        end interface
        
contains
        subroutine verlet_force(N, pos, vel, F, m, h)
                integer, intent(in) :: N;
                procedure (force_func), pointer, intent(in) :: F;
                real(8), dimension(3,N), intent(inout) :: pos, vel;
                real(8), dimension(N), intent(in) :: m;
                real(8), intent(in) :: h;
                real(8), dimension(3,N) :: force1, force2;
                integer :: x;

                force1 = F(N,pos);
                do x = 1,N
                        pos(:,x) = pos(:,x) + h*vel(:,x) + h**2.d0/(2.d0*m(x)) * force1(:,x);
                enddo
                force2 = F(N,pos);
                do x = 1,N
                        vel(:,x) = vel(:,x) + h/(2.d0*m(x))*(force2(:,x) + force1(:,x));
                enddo
        end subroutine
        
        subroutine verlet_potential(N, pos, vel, V, m, h)
                use differentiation;
                integer, intent(in) :: N;
                procedure (potential_func), pointer, intent(in) :: V;
                real(8), dimension(3,N), intent(inout) :: pos, vel;
                real(8), dimension(N), intent(in) :: m;
                real(8), intent(in) :: h;
                procedure (force_func), pointer :: F;
                F => diff_pot;
                call verlet_force(N, pos, vel, F, m, h)
                        
                contains              
                function diff_pot(l,posi)
                		use differentiation;
                        real(8), dimension(3,N) :: diff_pot;
                        integer, intent(in) :: l;
                        real(8), dimension(3,N), intent(in) :: posi;
                        diff_pot = -grad(V,l,posi);
                end function diff_pot
        end subroutine
        
end module verlet
