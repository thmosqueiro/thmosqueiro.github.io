! ===============================================
! 
! Programa para calcular a media de N numeros dados.
! Neste codigo, utilizo tanto a formatacao de saida
! como tambem a alocacao dinamica para que haja um
! exemplo pratico destes dois conceitos.
!
!
! Thiago Mosqueiro @ 2013
! Licensing: WTFPL
! tl;dr: "Do whatever you want, don't sue me'
!
! ===============================================
       
!     Vetor para guardar os numeros
       REAL, ALLOCATABLE :: nums(:)
       
!     Pedindo o numero de valores para tirar media
       WRITE(*,*) " Calcular a media de quantos numeros?"
       READ(*,*) N
       
!     Alocando o vetor
       ALLOCATE( nums(N) )
       
!     Guardando os valores para tirar a media
       WRITE(*,*) " Forneca numeros entre 0.0 e 10.0:"
       READ(*,*) nums
       
       
!     Variavel onde ficara a media
       amedia = 0.0
       
!     Calculando a media
       DO j = 1, N
          amedia = amedia + nums(j)
       ENDDO
       amedia = amedia/N
       
!     Escrevendo o resultado na tela
       WRITE(*,'(A8, F6.3)') " Media: ", amedia
       
!     Desalocando o vetor
       DEALLOCATE( nums )
       
       STOP
       END
