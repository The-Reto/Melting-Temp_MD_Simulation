program analysis
        implicit none;
        real(8), dimension(2,1000) :: temp; 
        real(8) :: a,b,c,d, avg_hot, avg_cold, avg, s2_hot, s2_cold, s2;
        integer :: x;
        open(1, FILE="./OUTPUT_HOT/MDiter");
        open(2, FILE="./OUTPUT_COLD/MDiter");
        do x = 1,1000
                read(1,*);
                read(2,*);
        enddo
        avg_hot = 0; avg_cold = 0; avg = 0;
        do x = 1,1000
                read(1,*) a,b,c,d,temp(1,x);
                read(2,*) a,b,c,d,temp(2,x);
                avg_hot = avg_hot + temp(1,x);
                avg_cold = avg_cold + temp(2,x);
                avg = avg + temp(1,x) + temp(2,x);
        enddo
        close(1); close(2);
        avg_hot = avg_hot/1000.d0;
        avg_cold = avg_cold/1000.d0;
        avg = avg/2000.d0;
        s2_hot = 0; s2_cold = 0; s2 = 0;
        do x = 1,1000
                s2_hot = s2_hot + (temp(1,x) - avg_hot)**2.d0;
                s2_cold = s2_cold + (temp(2,x) - avg_cold)**2.d0;
                s2 = s2 + (temp(1,x) - avg)**2.d0 + (temp(2,x) - avg)**2.d0;
        enddo
        s2_hot = s2_hot/1000.d0;
        s2_cold = s2_cold/1000.d0;
        s2 = s2/2000.d0;
        open(1, FILE="./RESULTS/RESULT.txt");
        write(1,'(A4,7x,A8,3x,A)') "FILE", "AVG_TEMP", "STD-DEVIATION";
        write(1,'(A4,7x,F7.2,4x,F5.2)') "HOT:", avg_hot, sqrt(s2_hot);
        write(1,'(A5,6x,F7.2,4x,F5.2)') "COLD:", avg_cold, sqrt(s2_cold);
        write(1,'(A8,3x,F7.2,4x,F5.2)') "OVERALL:", avg, sqrt(s2);
        close(1);
        write(*,'(A4,7x,A8,3x,A)') "FILE", "AVG_TEMP", "STD-DEVIATION";
        write(*,'(A4,7x,F7.2,4x,F5.2)') "HOT:", avg_hot, sqrt(s2_hot);
        write(*,'(A5,6x,F7.2,4x,F5.2)') "COLD:", avg_cold, sqrt(s2_cold);
        write(*,'(A8,3x,F7.2,4x,F5.2)') "OVERALL:", avg, sqrt(s2);
end program
