program part_1d
	use matcrs_mod
	use array
	implicit none
	type(matcrs) :: a
	integer :: i, j, k, np, npp, si
	character(len=16) :: prefix, fname, fname2
	!pn : 各点がどのプロセッサに所属するか
	!pc : 各プロセッサが他のプロセッサから受け取る数
	!ln : 各プロセッサ内でのローカルな添え字
	integer, allocatable :: part(:), pn(:), pc(:,:), ln(:,:), ext(:,:), next(:)
	type(matcrs_part) :: ap
	
	!分ける数, 接頭辞
	read *, np, prefix
	
	a = read_matcrs_array()
	
	allocate(part(a%n), pn(a%n), pc(np,0:np), ln(np,a%n), ext(np,np), next(np))
	
	!part初期化
	do i=1, a%n
		part(i) = i
	end do
	
	!pn初期化
	si = 0
	do i=1, np
		npp = ceiling(real(i)*(a%n)/np) - si
		pn(si+1:si+npp) = i
		si = si + npp
	end do
	
	!pc初期化
	pc = 0
	
	!ln初期化
	ln = 0
	
	print *, "partitioning"
	si = 0
	do i=1, np
		npp = ceiling(real(i)*(a%n)/np) - si
		
		ap = part_matcrs(a, part(si+1:si+npp), part(si+1:si+npp))
		
		write(fname, '(a,i2.2,".dat")') trim(prefix), i
		
		open(21, file=fname, status='replace')
		call write_matcrs_part_array(21, ap)
		
		do j=1, ap%ext
			k = pn(ap%map(ap%inn+j))
			pc(k,i) = pc(k,i) + 1
		end do
		
		do j=1, ap%inn+ap%ext
			ln(i,ap%map(j)) = j
		end do
		
		close(21)
		
		si = si + npp
	end do
	
	do i=1, np
		do j=1, np
			pc(i,j) = pc(i,j-1) + pc(i,j)
		end do
	end do
	
	ext = 0
	do i=1, np
		k = 0
		do j=1, np
			if(pc(i,j) /= pc(i,j-1)) then
				k = k + 1
				pc(i,k) = pc(i,j)
				ext(i,k) = j
			end if
		end do
		next(i) = k
	end do
	
	do i=1, np
		write(fname, '(a,i2.2,".dat")') trim(prefix), i
		
		!print *, fname, "open"
		open(21, file=fname, status='old')
		ap = read_file_matcrs_part_array(21)
		close(21)
		
		open(21, file=fname, access='append')
		write(21, *) next(i)
		write(21, *) ext(i,:next(i))
		write(21, *) pc(i,:next(i))
		close(21)
	end do
	
	print *, "send"
	!送信すべき値
	do i=1, np
		write(fname, '(a,i2.2,".dat")') trim(prefix), i
		
		!print *, fname, "open"
		open(21, file=fname, status='old')
		ap = read_file_matcrs_part_array(21)
		read(21, *) next(i)
		read(21, *) ext(i,:next(i))
		read(21, *) pc(i,:next(i))
		close(21)
		
		!print *, "vvv"
		do j=1, ap%ext
			!print *, ap%inn+j
			!print *, ap%map
			!print *, ap%map(ap%inn+j)
			k = pn(ap%map(ap%inn+j))
			write(fname2, '(a,i2.2,".dat")') trim(prefix), k
			!print *, fname2, "open"
			open(21, file=fname2, access='append')
			write(21, *) ln(k,ap%map(ap%inn+j))
			close(21)
		end do
	end do
	
	print *, "recv"
	ext = 0
	pc = 0
	!受信すべき値
	do i=1, np
		!print *, fname, "open"
		write(fname, '(a,i2.2,".dat")') trim(prefix), i
		
		open(21, file=fname, status='old')
		ap = read_file_matcrs_part_array(21)
		close(21)
		
		do j=1, ap%ext
			k = pn(ap%map(ap%inn+j))
			pc(i, k) = pc(i, k) + 1
		end do
		
		do j=1, np
			pc(i,j) = pc(i,j-1) + pc(i,j)
		end do
		
		k = 0
		do j=1, np
			if(pc(i,j) /= pc(i,j-1)) then
				k = k + 1
				pc(i,k) = pc(i,j)
				ext(i,k) = j
			end if
		end do
		next(i) = k
		
		open(21, file=fname, access='append')
		write(21, *) next(i)
		write(21, *) ext(i,:next(i))
		write(21, *) pc(i,:next(i))
		
		do j=1, ap%ext
			write(21, *) ap%inn+j
		end do
		
		close(21)
	end do
end program