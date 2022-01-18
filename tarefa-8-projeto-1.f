      program tarefa8

c     numero de pontos que serão utilizados na simulação
      parameter (m = 1000)
      n_pts = 0
      write(*,*)'Em qual dimensão deseja calcular o volume?'
      read(*,*) id
      
      do i = 1, m
c       r será o quadrado da distância de um ponto à origem
        r = 0
        do j = 1, id
c         coord sempre será um valor entre -1 e 1
          coord = 2*rand() - 1
          r = r + coord**2
        end do
c       verifica se o ponto está no interior da esfera de raio 1
        if (sqrt(r).le.1.0) n_pts = n_pts + 1
      end do

      write(*,*)'O volume da esfera unitária é:', n_pts*2.0**id/m

      end
