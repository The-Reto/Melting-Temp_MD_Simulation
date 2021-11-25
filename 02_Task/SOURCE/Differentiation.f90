module differentiation
        implicit none;
        
        interface
                function diff_func(x)
                        real(8) :: diff_func;
                        real(8), intent(in) :: x;
                end function diff_func
                        
                function grad_func(N, pos)
                        real(8) :: grad_func;
                        integer, intent(in) :: N;
                        real(8), dimension(3,N), intent(in) :: pos;
                end function grad_func
        end interface
contains
        function diff_function(f, x0)
                real(8) :: diff_function;
                real(8), intent(in) :: x0;
                procedure (diff_func), pointer, intent(in) :: f;        
                real(8), dimension(3) :: ci = (/1.d0, 0.d0, -1.d0/);

                real(kind=8) :: h = 1.d-3;
                integer :: k, l = size(ci);
!                diff_function = (f(x0-h) - f(x0+h))/(2.d0*h);
                diff_function = 0;
                do k = -(l/2), l/2
                        diff_function = diff_function + (ci(k+l/2+1)*f(x0 + k*h))/(2.d0*h);
                enddo
        end function diff_function
        
        function grad(V, N, pos)
                real(8), dimension(3,N) :: grad;
                integer, intent(in) :: N;
                real(8), dimension(3,N), intent(in) :: pos;  
                procedure (grad_func), pointer, intent(in) :: V;
                integer :: x,y;
                procedure (diff_func), pointer :: F;
                F => diff;
                do x = 1,3
                        do y = 1,N
                                grad(x,y) = diff_function(F, pos(x,y));
                        enddo
                enddo
                contains
                function diff(x0)
                        real(8) :: diff;
                        real(8), intent(in) :: x0;
                        real(8), dimension(3,N) :: dpos;
                        dpos = pos;
                        dpos(x,y) = x0;
                        diff = V(N, dpos);
                end function diff
        end function grad
end module differentiation
























