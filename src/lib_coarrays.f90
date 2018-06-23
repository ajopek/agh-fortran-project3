module lib_coarrays 
    contains
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
end module
    