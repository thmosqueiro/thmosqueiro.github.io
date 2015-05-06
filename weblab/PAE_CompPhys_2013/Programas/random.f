! ===============================================
! 
! Este programa serve para facilitar o uso de SEEDS
! com numeros aleatorios. Ao rodar um programa, voce
! pode usar a seguinte sintaxe:
!
!     ./meuprograma $RANDOM
!
! Na linha de comando (shell), existe uma variavel 
! $RANDOM que é sempre diferente. Apesar de nao ser
! um bom gerador de numeros aleatorios (por exemplo
! para aplicacoes em encriptacao), eh excelente para
! nosso proposito. Lembre-se que em vez de rand(),
! voce terah que chamar numeros aleatorios usando
! ran1().
!
! Importante: se voce estiver trabalhando com dupla 
! precisao, redefina ran1 para real*8
!
!
! Thiago Mosqueiro @ 2013
! Licensing: WTFPL
! tl;dr: "Do whatever you want, don't sue me'
!
! ===============================================
      
      
! -- Variaveis necessarias para setar o seed
      character(len=20)      :: bugger
      integer, allocatable :: jseed(:)
      real ran1
      
      
! -- Setting up the random numbers ----     
      call getarg(1, bugger)
      read(bugger,*) jsizer
      jr=12
      allocate( jseed(jr) )
      CALL RANDOM_SEED (SIZE=jr)
      jseed = jsizer + 37 * (/ (i - 1, i = 1, jr) /)
      CALL RANDOM_SEED (PUT=jseed)
      deallocate(jseed)
! -- End setting up random numbers ----
     
            
!     Rode o mesmo codigo diversas vezes passando
!     seeds diferentes. Veja o que acontece quando voce
!     passa o mesmo seed.
      write(*,*) ran1()
      
      stop
      end
      
      
!
!     Funacao RAN1()
!     
!     Usando a funcao rotina mais nova do Fortran 95 (ou mais)
!     para gerar numeros aleatorios
!     
      real function ran1() result(x)
      
      CALL random_number(x)
      return
      
      end function ran1
      
