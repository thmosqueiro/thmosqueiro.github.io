! ===============================================
! 
! Este programa serve como exemplo para alocacao dinamica,
! quando voce quer que o vetor ou matriz tenham dimensoes
! diferentes conforme a entrada. Por exemplo, se voce precisa
! ler N numeros e armazena-los em um vetor, voce pode (i) criar
! um vetor de milhares de componentes e soh utilizar as N
! primeiras ou (ii) criar um vetor sem numero de componentes
! definido, pedir por N e "alocar" N dimensoes para o vetor.
! Assim nao ha disperdicio de memoria.
!
!
! Thiago Mosqueiro @ 2013
! Licensing: WTFPL
! tl;dr: "Do whatever you want, don't sue me'
!
! ===============================================


!     Primeiro voce tem que dizer que o seu vetor (ou matriz)
!     serah alocado mais tarde. Isso se faz utilizando a instrucao
!     seguite:
       REAL, ALLOCATABLE :: amat(:,:)
       
!     Neste caso, tenho uma matriz (chamada amat) que tem duas
!     dimensoes, embora ainda nao saibamos quantos elementos 
!     cada dimensao tenha.
       
!     Vamos pedir para que o usuario determine esse numero de elementos.
       WRITE(*,*) "Forneca o numero de elemtnos na primeira dimensao:"
       READ(*,*) N1
       WRITE(*,*) "Forneca o numero de elemtnos na primeira dimensao:"
       READ(*,*) N2
       
!     Com os valores em N1 e N2, agora alocamos a matriz
       ALLOCATE( amat(N1, N2) )
       
!     Soh para fazer algum calculo teste
       pi = acos(-1.)
       aux1 = 2*pi/N1
       aux2 = 2*pi/N2
       
       DO j = 1, N1
          DO k = 1, N2
             amat(j, k) = sin( aux1*float(j) )*sin( aux2*float(k) )
          ENDDO
       ENDDO
       
!     Imprimindo num arquivo os resultados
       WRITE(1,*) amat
       
       
!     Essa parte nao eh necessaria, mas pode ser usada caso
!     haja uso excessivo de memoria
       DEALLOCATE( amat )
       
       
       STOP
       END
      
