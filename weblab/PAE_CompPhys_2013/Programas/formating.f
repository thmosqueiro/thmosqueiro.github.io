! ===============================================
! 
! Este programa serve para poder testar diferentes 
! formatacoes de saida de variaveis.
! Tente comprender a saida do programa e depois mude
! os formatos preh-definidos.
!
!
! Thiago Mosqueiro @ 2013
! Licensing: WTFPL
! tl;dr: "Do whatever you want, don't sue me'
!
! ===============================================
       
       WRITE(*,*) "Pick a real number from 0 to 9.99"
       
!     Pegando um valor do teclado e guardando
!     na variavel value
       READ(*,*) value
       
!     Tomando a parte inteira da variavel
       ivalue = int( value )
       
       
!     Escrevendo a variavel usando diferentes formatos
       WRITE(*,*)
       WRITE(*,*) "Writing on the screen the number in diferent ways"
       WRITE(*,*) value
       WRITE(*,'(F6.3)') value
       WRITE(*,'(F5.3)') value
       WRITE(*,'(F5.2)') value
       WRITE(*,'(F3.2)') value
      
      
!     Mesma coisa com a parte inteira
       WRITE(*,*)
       WRITE(*,*) "Now for the integer"
       WRITE(*,*) ivalue
       WRITE(*,'(I5)') ivalue
       WRITE(*,'(I3)') ivalue
       
       
!     Imprimindo tanto uma variavel inteira, como uma real
!     ao mesmo tempo.
       WRITE(*,*)
       WRITE(*,*) "Writing both" 
       WRITE(*,'(F5.3, I3)') value, ivalue
       
       
       STOP
       END
