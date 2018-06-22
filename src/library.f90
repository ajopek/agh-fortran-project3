module library   
contains
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

end module library
