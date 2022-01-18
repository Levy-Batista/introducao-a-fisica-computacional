      program tarefa4
 
      double precision xd, y2, cosh2, tmp2
c     xr, y1, cosh1, tmp1 são as variáveis análogas de simples precisão
      parameter (eprec1 = 1.e-5, eprec2 = 1.d-13)
c     precisão para o cosh em simples e dupla precisão, respectivamente

      y1 = 1.e0
      y2 = 1.d0
      tmp1 = 1.e0
      i = 1
      j = 1
      tmp2 = 1.d0
      write(*,*)'Digite o valor x do qual se deseja calcular cosh:'
      read(*,*)xr
      xd = dble(xr)
      cosh1 = cosh(xr)
      cosh2 = dcosh(xd)

c     atualiza o valor de y1 até que fique dentro da precisão eprec1      
 10   if (abs(y1 - cosh1).gt.eprec1) then
      	tmp1 = tmp1*xr**2/(2*i*(2*i-1))	
        y1 = y1 + tmp1
        i = i + 1
        goto 10
      endif

      write(*,*)'O valor de cosh(x) é (precisão simples):', y1
   
c     atualiza o valor de y2 até que fique dentro da precisão eprec2
 20   if (abs(y2 - cosh2).gt.eprec2) then
      	tmp2 = tmp2*xd**2/(2*j*(2*j-1))	
        y2 = y2 + tmp2
        j = j + 1
        goto 20
      endif

      write(*,*)'O valor de cosh(x) é (precisão dupla):', y2

      end
