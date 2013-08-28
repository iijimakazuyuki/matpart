program part3d2d
	implicit none
	integer :: p, d, i, j, si, ei, sj, ej, x, y, z
	read *, p, d
	do j = 1, p
		do i = 1, p
			si = (i-1)*d/p + 1
			ei = i*d/p
			sj = (j-1)*d/p + 1
			ej = j*d/p
			print *, (ej - sj + 1) * (ei - si + 1) * d
			do z = 1, d
				do y = sj, ej
					do x = si, ei
						print *, (z-1)*d*d + (y-1)*d + x
					end do
				end do
			end do
		end do
	end do
end program
