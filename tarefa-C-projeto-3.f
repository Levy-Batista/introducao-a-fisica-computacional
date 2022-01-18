      program tarefaC
      
      real*8 precisao, a, b, c, d
c     definindo a precisão como uma variável comum a ser utilizada nas demais funções
      common /cte/precisao
      precisao = 1.d-6
      a = -10.d0
      b = -10.d0
      c = -9.d0
      d = -10.d0        

c     os métodos foram definidos por meio de sub-rotinas
      call procura(a)
      call newton(b)
      call secante(c, d)      

      end

c     sub-rotina que aplica o método de busca direta
      subroutine procura (r)
        real*8 r, raux, h, precisao
        common /cte/precisao
c       i marca o número de iterações
        i = 0
c       h é o intervalo inicial utilizado
        h = 0.3d0
c       raux marca o primeiro r que muda o sinal do polinômio
        raux = r + h
        open(1, file='1saida-C-11212550')
        
        write(1,*)"Método de busca direta"
        write(1,100)"Iteração", "|", "r1", "|"
c       itera até o módulo do valor do polinômio estar dentro da precisão
 15     if (abs((((r+raux)/2)**3 - 14*((r+raux)/2) - 20)).gt.precisao)
     +    then
c         escreve o valor da raíz para cada iteração
          write(1,200)i, "|", (raux+r)/2, "|"
c         muda os valores de r e raux antes de "refinar" a raíz
          if (i.ne.0) then
            r = raux
            raux = raux + h
          end if
c         procura o primeiro raux que inverte o sinal do polinômio
 10       if ((r**3 - 14*r - 20)*(raux**3 - 14*raux - 20).gt.0) then
            raux = raux + h
            goto 10
          end if
c         divide pela metade o tamanho do intervalo a cada iteração
          r = raux - h
          h = -h/2
          i = i + 1
          goto 15
        end if
        write(1,200)i, "|", (raux+r)/2, "|"
        write(1,300)"exato: ", 1 - dsqrt(11.d0) 

c       o processo é repetido mais duas vezes para encontrar o valor das outras raízes
c       r assume um novo valor e i e h são "resetados"
        r = -2.2d0
        i = 0
        h = 0.3d0
        raux = r + h
        write(1,*)" "
        write(1,100)"Iteração", "|", "r2", "|"
 25     if (abs((((r+raux)/2)**3 - 14*((r+raux)/2) - 20)).gt.precisao)
     +    then
          write(1,200)i, "|", (raux+r)/2, "|"
          if (i.ne.0) then
            r = raux
            raux = raux + h
          end if
 20       if ((r**3 - 14*r - 20)*(raux**3 - 14*raux - 20).gt.0) then
            raux = raux + h
            goto 20
          end if
          r = raux - h
          h = -h/2
          i = i + 1
          goto 25
        end if
        write(1,200)i, "|", (raux+r)/2, "|"
        write(1,300)"exato: ", -2.d0

        r = 0.d0
        i = 0
        h = 0.3d0
        raux = r + h
        write(1,*)" "
        write(1,100)"Iteração", "|", "r3", "|"
 35     if (abs((((r+raux)/2)**3 - 14*((r+raux)/2) - 20)).gt.precisao)
     +    then
          write(1,200)i, "|", (raux+r)/2, "|"
          if (i.ne.0) then
            r = raux
            raux = raux + h
          end if
 30       if ((r**3 - 14*r - 20)*(raux**3 - 14*raux - 20).gt.0) then
            raux = raux + h
            goto 30
          end if
          r = raux - h
          h = -h/2
          i = i + 1
          goto 35
        end if
        write(1,200)i, "|", (raux+r)/2, "|"
        write(1,300)"exato: ", 1 + dsqrt(11.d0)

c       formatação esperada no arquivo de saída
 100    format(A10, A, A11, A)
 200    format(I8, A, F11.7, A)
 300    format(A, F10.7)
        close(1)
        return
      end 

c     sub-rotina que aplica o método de Newton-Raphson
      subroutine newton (r)
        real*8 r, precisao
        common /cte/precisao
c       j marca o número de iterações
        j = 0 
        open(2, file='2saida-C-11212550')

        write(2,*)"Método de Newton-Raphson"
        write(2,100)"Iteração", "|", "r1", "|"
c       itera até o módulo do valor do polinômio estar dentro da precisão
 40     if (abs(r**3 - 14*r - 20).gt.precisao) then
          write(2,200)j, "|", r, "|"  
          r = r - (r**3 - 14*r - 20)/(3*r**2 - 14)
          j = j + 1
          goto 40
        end if
        write(2,200)j, "|", r, "|"
        write(2,300)"exato: ", 1 - dsqrt(11.d0)
  
c       os valores de r e j são "resetados"
c       o processo é repetido duas vezes para encontrar as outras raízes 
        r = 0.d0
        j = 0
        write(2,*)" "
        write(2,100)"Iteração", "|", "r2", "|"
 50     if (abs(r**3 - 14*r - 20).gt.precisao) then
          write(2,200)j, "|", r, "|"  
          r = r - (r**3 - 14*r - 20)/(3*r**2 - 14)
          j = j + 1
          goto 50
        end if
        write(2,200)j, "|", r, "|" 
        write(2,300)"exato: ", -2.d0

        r = 10.d0
        j = 0
        write(2,*)" "
        write(2,100)"Iteração", "|", "r3", "|"
 60     if (abs(r**3 - 14*r - 20).gt.precisao) then
          write(2,200)j, "|", r, "|"  
          r = r - (r**3 - 14*r - 20)/(3*r**2 - 14)
          j = j + 1
          goto 60
        end if
        write(2,200)j, "|", r, "|" 
        write(2,300)"exato: ", 1 + dsqrt(11.d0)         

 100    format(A10, A, A11, A)
 200    format(I8, A, F11.7, A)
 300    format(A, F10.7)
        close(2)
        return
      end 
     
c     sub-rotina que aplica o método da secante
      subroutine secante (r1, r2)
c       r1 e r2 são os chutes iniciais
c       tmp é uma variável auxiliar necessária na hora de atualizar r1 e r2
        real*8 r1, r2, tmp, precisao
        common /cte/precisao
c       k marca o número de iterações
        k = 0
        open(3, file='3saida-C-11212550')

        write(3,*)"Método da secante"
        write(3,100)"Iteração", "|", "r1", "|"
c       itera até o módulo do valor do polinômio estar dentro da precisão
 70     if (abs(r2**3 - 14*r2 - 20).gt.precisao) then
          write(3,200)k, "|", r2, "|"
          tmp = r2
          r2 = r2 - (r2**3 - 14*r2 - 20)*(r2 - r1)/
     +             (r2**3 - 14*r2 - r1**3 + 14*r1)
          r1 = tmp
          k = k + 1
          goto 70
        end if
        write(3,200)k, "|", r2, "|"
        write(3,300)"exato: ", 1 - dsqrt(11.d0)

c       os valores de r1, r2 e k são "resetados"
c       o processo é repetido duas vezes para encontrar as outras raízes
        r1 = -1.d0
        r2 = 0.d0
        k = 0
        write(3,*)" "
        write(3,100)"Iteração", "|", "r2", "|"
 80     if (abs(r2**3 - 14*r2 - 20).gt.precisao) then
          write(3,200)k, "|", r2, "|"
          tmp = r2
          r2 = r2 - (r2**3 - 14*r2 - 20)*(r2 - r1)/
     +             (r2**3 - 14*r2 - r1**3 + 14*r1)
          r1 = tmp
          k = k + 1
          goto 80
        end if
        write(3,200)k, "|", r2, "|"
        write(3,300)"exato: ", -2.d0

        r1 = 9.d0
        r2 = 10.d0
        k = 0
        write(3,*)" "
        write(3,100)"Iteração", "|", "r3", "|"
 90     if (abs(r2**3 - 14*r2 - 20).gt.precisao) then
          write(3,200)k, "|", r2, "|"
          tmp = r2
          r2 = r2 - (r2**3 - 14*r2 - 20)*(r2 - r1)/
     +             (r2**3 - 14*r2 - r1**3 + 14*r1)
          r1 = tmp
          k = k + 1
          goto 90
        end if
        write(3,200)k, "|", r2, "|"
        write(3,300)"exato: ", 1 + dsqrt(11.d0)
      
 100    format(A10, A, A11, A)
 200    format(I8, A, F11.7, A)
 300    format(A, F10.7)
        close(3)
        return
      end 
