module library   
contains
#ifdef BASIC
    !---------------------------------------------------------------------------  
    !> @author 
    !> Artur Jopek
    !
    ! DESCRIPTION: 
    !> Performs gaussian elimination on square matricies
    !> @param[in] coef_matrix - coefitients matrix
    !> @param[out] val_matrix - values matrix
    !> @param[int] matrix_dim - matrix dimmension     
    !--------------------------------------------------------------------------- 
    subroutine gaussian_elimination(coef_matrix, val_matrix, matrix_dim)
        implicit none
        integer, intent(in) :: matrix_dim
        real (kind = 8), dimension(matrix_dim, matrix_dim) :: coef_matrix
        real (kind = 8), dimension(matrix_dim) :: val_matrix

        integer :: i, j
        real (kind = 8) :: multiplier
        do i = 1, matrix_dim
            do j = 1, matrix_dim
                if (i .NE. j) then
                    multiplier = coef_matrix(i, j) / coef_matrix(i, i)
                    coef_matrix(:, j) = coef_matrix(:, j) - multiplier * coef_matrix(:, i)
                    val_matrix(j) = val_matrix(j) - multiplier * val_matrix(i)
                end if
                val_matrix(i) = val_matrix(i) / coef_matrix(i, i)
                coef_matrix(:, i) = coef_matrix(:, i) / coef_matrix(i, i)
            end do
        end do
    end subroutine gaussian_elimination
    !---------------------------------------------------------------------------  
    !> @author 
    !> Artur Jopek
    !
    ! DESCRIPTION: 
    !> Multiply maticies 
    !> @brief
    !> @param[in] first - first matrix
    !> @param[in] second - second matrix      
    !> @param[out] multiply - result of multiplication
    !> @param[out] status - 0 if ok, 1 if error
    !--------------------------------------------------------------------------- 
    subroutine mm(first, second, multiply, status)
        implicit none
        real (kind = 8), intent(in)     :: first(:,:) 
        real (kind = 8), intent(in)     :: second(:,:)
        real (kind = 8), intent(out)    :: multiply(:,:)
        integer (kind = 4), intent(out) :: status
        real (kind = 8) :: start, finish
        integer :: i, j, k ! Iterators
        integer :: shape_first(2), shape_second(2) ! Matrix sizes

        shape_first = SHAPE(first)
        shape_second = SHAPE(second)
        multiply(:, :) = 0

        if (shape_second(1) .EQ. shape_first(2)) then        
            do i=1, shape_first(1)
                do j=1, shape_second(2)
                    multiply(i, j) = DOT_PRODUCT(first(i,:), second(:,j))
                end do
            end do
            status = 0
        else 
            status = 1
        end if
end subroutine
#elif COARRAY
    !---------------------------------------------------------------------------  
    !> @author 
    !> Artur Jopek
    !
    ! DESCRIPTION: 
    !> Multiply maticies using coarrays
    !> @brief
    !> @param[in] first - first matrix
    !> @param[in] second - second matrix      
    !> @param[out] multiply - result of multiplication
    !> @param[out] status - 0 if ok, 1 if error
    !--------------------------------------------------------------------------- 
    subroutine mm(first, second, multiply, status)
        implicit none
        real (kind = 8), intent(in)     :: first(:,:) 
        real (kind = 8), intent(in)     :: second(:,:)
        real (kind = 8), intent(out)    :: multiply(:,:)
        real ( kind = 8), codimension[:], dimension(:,:), allocatable :: intermediate
        integer (kind = 4), intent(out) :: status
        real (kind = 8) :: start, finish
        integer :: row, col, img_num ! Iterators
        integer :: shape_first(2), shape_second(2) ! Matrix sizes
        integer ( kind = 4), codimension[:], allocatable :: begin_row, end_row ! Markers for each image
        integer :: chunck ! Size of chunck to process for each image
        shape_first = SHAPE(first)
        shape_second = SHAPE(second)
        multiply(:, :) = 0

        allocate(begin_row[*])
        allocate(end_row[*])

        chunck = CEILING(real(shape_first(1))/NUM_IMAGES())

        ! Min in both to not get out of matrix size, if rows % NUM_IMAGES != 0
        begin_row = MIN(shape_first(1), (THIS_IMAGE() - 1) * chunck) + 1
        end_row   = MIN(shape_first(1), THIS_IMAGE() * chunck)

        allocate(intermediate(chunck, shape_second(2))[*])

        ! Check sizes
        if (shape_second(1) .NE. shape_first(2)) then        
            status = 1 ! Cannot multiply, bad input sizes
            return
        else 
            status = 0
        end if

        do row = begin_row, end_row
            do col=1, shape_second(2)
                intermediate(row - begin_row + 1, col) = DOT_PRODUCT(first(row, :), second(:, col))
            end do
        end do

        ! Gather results from images
        if(THIS_IMAGE() .EQ. 1) then
            do img_num = 1, NUM_IMAGES()
                multiply(begin_row[img_num]:end_row[img_num], :) &
                 = intermediate(1:(begin_row[img_num] - end_row[img_num] + 1), :) &
                 [img_num]
            end do
        end if
    end subroutine

    !---------------------------------------------------------------------------  
    !> @author 
    !> Artur Jopek
    !
    ! DESCRIPTION: 
    !> Performs gaussian elimination on square matricies
    !> @param[in] coef_matrix - coefitients matrix
    !> @param[out] val_matrix - values matrix
    !> @param[int] matrix_dim - matrix dimmension     
    !--------------------------------------------------------------------------- 
    subroutine gaussian_elimination(A, X, n)
        integer(kind=4), intent(in) :: n
        real(kind = 8), intent(inout) :: A(0:N, 0:N), X(0:N)
        real ( kind = 8), codimension[:], allocatable :: coA(:,:)
        real ( kind = 8), codimension[:], allocatable :: coX(:)
        real(kind = 8) :: ratio
        integer(kind=8) :: i, j

        allocate(coA(0:N, 0:N)[*])
        allocate(coX(0:N)[*])

        if (THIS_IMAGE() .eq. 1) then
            coA(:,:)[1] = A(:,:)
            coX(:)[1] = X(:)
        end if

        do i = 0, N
            ! scale to get 1's on diagonal
            if(THIS_IMAGE() .eq. 1) then
                coX(i)[1] = coX(i)[1] / coA(i, i)[1]
                coA(:, i)[1] = coA(:, i)[1] / coA(i, i)[1]
            end if
            sync all
            do j = THIS_IMAGE() - 1, N, NUM_IMAGES()
                if ((i .NE. j) .AND. (ABS(coA(i, i)[1] - 0) > 1d-6)) then
                    ratio = coA(i, j)[1] / coA(i, i)[1]
                    coA(:,j)[1] = coA(:,j)[1] - ratio * coA(:, i)[1]
                    coX(j)[1] = coX(j)[1] - ratio * coX(i)[1]
                end if
            end do
        end do
        

        if (THIS_IMAGE() .eq. 1) then
          A(:,:) = coA(:,:)[1] 
          X(:) = coX(:)[1] 
        end if

        deallocate(coA)
        deallocate(coX)
      end subroutine

#endif

end module library
