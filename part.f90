program part
	use matcrs_mod
	use bitset_mod
	implicit none
	type(matcrs) :: a
	type(bitset), allocatable :: p(:)
	integer :: i, j, k, np
	character(len=16) :: prefix, fname, fname2
	!pn : 各点がどのプロセッサに所属するか
	!pc : 各プロセッサが他のプロセッサから受け取る数
	!ln : 各プロセッサ内でのローカルな添え字
	integer, allocatable :: npp(:), pt(:), pn(:), pc(:,:), ln(:,:), ext(:,:), next(:)
	type(matcrs_part) :: ap
	
	read *, np, prefix
	
	a = read_matcrs_array()
	
	allocate(p(np))
	
	do i=1, np
		p(i) = new_bitset(a%n)
		call read_bitset_list(p(i))
	end do
	
	allocate(npp(np), pn(a%n), pc(np,0:np), ln(np,a%n), ext(np,np), next(np))
	
	npp = 0
	
	!pn初期化
	do i=1, np
		j = 0
		do
			j = next_bitset(p(i), j)
			if(j == 0) exit
			pn(j) = i
			npp(i) = npp(i) + 1
		end do
	end do
	
	!pc初期化
	pc = 0
	
	!ln初期化
	ln = 0
	
	!print *, "partitioning"
	do i=1, np
		allocate(pt(npp(i)))
		
		j = 0
		call assign_bitset_array(pt, j, p(i))
		
		ap = part_matcrs(a, pt, pt)
		
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
		
		deallocate(pt)
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
	
	!print *, "send"
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
	
	!print *, "recv"
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
