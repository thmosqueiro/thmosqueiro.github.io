!First Program -  Edmilson Roque - 7978737
		
	
	write(*,*)'Type a value in radians to see its cosine' 
	read(*,*) x

	tcos = 1
		
	tcosr = 1

	i = 2

	j = 1

	ifat = 2
	
	vm = 100 
 

	do while (vm .gt. 0.000001)
		               
		tcos = (((-1)**(i-j))* (x)**(2*(i-j)))/(ifat)
		
		tcosaux = tcosr

		tcosr = tcosr + tcos	
		
		i = i + 2

		j = j + 1
		
		ifat = i * (i - 1) * ifat
		
		vm = tcosr - tcosaux
                vm = abs(vm)
	
		write(*,*) vm, tcosaux,tcosr, tcos

		
	end do

	write(*,*)'The correct taylor expansion is ', tcosr

	stop

	end

