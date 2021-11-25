module vector
	implicit none;

contains
	function norm_3d(vector)
        real(8) :: norm_3d;
        real(8), dimension(3), intent(in) :: vector;
        norm_3d = sqrt(scalar_product_3d(vector, vector));
	end function norm_3d

	function scalar_product_3d(vec1, vec2)
		    real(8) :: scalar_product_3d;
		    real(8), dimension(3), intent (in) :: vec1, vec2;
		    integer :: x;
		    scalar_product_3d =  vec1(1)*vec2(1) + vec1(2)*vec2(2) + vec1(3)*vec2(3);
	end function scalar_product_3d

	function norm(N, vector)
		    real(8) :: norm;
		    integer, intent(in) :: N;
		    real(8), dimension(3,N), intent(in) :: vector;
		    norm = sqrt(scalar_product(N, vector, vector));
	end function norm

	function scalar_product(N, vec1, vec2)
		    real(8) :: scalar_product;
		    integer, intent(in) :: N;
		    real(8), dimension(3,N), intent (in) :: vec1, vec2;
		    integer :: x;
		    scalar_product = 0;
		    do x = 1,N
		            scalar_product = scalar_product + vec1(1,x)*vec2(1,x) + vec1(2,x)*vec2(2,x) + vec1(3,x)*vec2(3,x);
		    enddo
	end function scalar_product
end module
