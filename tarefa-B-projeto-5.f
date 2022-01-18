      program tarefaB

      implicit real*8 (a-h, o-z)

c     itens b1 e b2
      alpha = 4.d0*dacos(-1.d0)**2
c     condições iniciais da Terra
      vTx0 = 0.d0
      vTy0 = dsqrt(alpha)
      xT = 1.d0
      yT = 0.d0
c     condições iniciais de Júpiter
      VJx0 = 0.d0
      vJy0 = dsqrt(alpha/5.2d0)
      xJ = 5.2d0
      yJ = 0.d0
      deltat = 0.01d0
c     tempo de movimento
      T = 0.d0
      open(1, file='1saida-B-11212550')
      open(2, file='2saida-B-11212550')
      write(1, 100)xT, yT
      write(2, 100)xJ, yJ      

c     início do método de Verlet para a Terra e Júpiter
      xTaux = xT
      yTaux = yT
      xT = xT + vTx0*deltat
      yT = yT + vTy0*deltat
      xJaux = xJ
      yJaux = yJ
      xJ = xJ + vJx0*deltat
      yJ = yJ + vJy0*deltat
      T = T + deltat
      write(1, 100)xT, yT
      write(2, 100)xJ, yJ

 10   if (T.le.20) then
c       distância da Terra ao Sol
        rT = dsqrt(xT**2 + yT**2)
c       distância da Terra a Júpiter
        rTJ = dsqrt((xT - xJ)**2 + (yT - yJ)**2)
c       método de Verlet para a Terra
        xTaux2 = xT
        yTaux2 = yT
c       o segundo termo é alterado quando a massa de júpiter é aumentada
        xT = 2*xT - xTaux - alpha*xT*deltat**2/rT**3 - (alpha/1000)*
     +      (xT - xJ)*deltat**2/rTJ**3
        yT = 2*yT - yTaux - alpha*yT*deltat**2/rT**3 - (alpha/1000)*
     +      (yT - yJ)*deltat**2/rTJ**3
        xTaux = xTaux2
        yTaux = yTaux2
c       distância de Júpiter ao Sol
        rJ = dsqrt(xJ**2 + yJ**2)
c       método de Verlet para Júpiter
        xJaux2 = xJ
        yJaux2 = yJ
        xJ = 2*xJ - xJaux - alpha*xJ*deltat**2/rJ**3 - (alpha/
     +      (3*10**5))*(xJ - xTaux)*deltat**2/rTJ**3
        yJ = 2*yJ - yJaux - alpha*yJ*deltat**2/rJ**3 - (alpha/
     +      (3*10**5))*(yJ - yTaux)*deltat**2/rTJ**3
        xJaux = xJaux2
        yJaux = yJaux2
        T = T + deltat
        write(1, 100)xT, yT
        write(2, 100)xJ, yJ
        goto 10
      end if
      close(1)
      close(2)

c     item b3
c     condições iniciais dos três asteróides
      v1x0 = 0.d0
      v1y0 = 3.628d0
      x1 = 3.000d0
      y1 = 0.d0
      v2x0 = 0.d0
      v2y0 = 3.471d0
      x2 = 3.276d0
      y2 = 0.d0
      v3x0 = 0.d0
      v3y0 = 3.267d0
      x3 = 3.700d0
      y3 = 0.d0
c     condições iniciais de Júpiter
      VJx0 = 0.d0
      vJy0 = 2.755d0
      xJ = 5.2d0
      yJ = 0.d0
      deltat = 0.01d0
      T = 0.d0
      open(6, file='3saida-B-11212550')
      open(7, file='4saida-B-11212550')
      open(8, file='5saida-B-11212550')
      open(9, file='6saida-B-11212550')
      write(6, 100)x1, y1
      write(7, 100)x2, y2
      write(8, 100)x3, y3
      write(9, 100)xJ, yJ

c     início da aplicação do método de Verlet
      x1aux = x1
      y1aux = y1
      x1 = x1 + v1x0*deltat
      y1 = y1 + v1y0*deltat
      x2aux = x2
      y2aux = y2
      x2 = x2 + v2x0*deltat
      y2 = y2 + v2y0*deltat
      x3aux = x3
      y3aux = y3
      x3 = x3 + v3x0*deltat
      y3 = y3 + v3y0*deltat
      xJaux = xJ
      yJaux = yJ
      xJ = xJ + vJx0*deltat
      yJ = yJ + vJy0*deltat
      T = T + deltat
      write(6, 100)x1, y1
      write(7, 100)x2, y2
      write(8, 100)x3, y3
      write(9, 100)xJ, yJ

 20   if (T.le.50) then
c       distância do asteróide I ao Sol
        r1 = dsqrt(x1**2 + y1**2)
c       distância do asteróide I a Júpiter
        r1J = dsqrt((x1 - xJ)**2 + (y1 - yJ)**2)
c       método de Verlet para o asteróide I
        x1aux2 = x1
        y1aux2 = y1
        x1 = 2*x1 - x1aux - alpha*x1*deltat**2/r1**3 - (alpha/1000)*
     +      (x1 - xJ)*deltat**2/r1J**3
        y1 = 2*y1 - y1aux - alpha*y1*deltat**2/r1**3 - (alpha/1000)*
     +      (y1 - yJ)*deltat**2/r1J**3
        x1aux = x1aux2
        y1aux = y1aux2
c       distância do asteróide II ao Sol
        r2 = dsqrt(x2**2 + y2**2)
c       distância do asteróide II a Júpiter
        r2J = dsqrt((x2 - xJ)**2 + (y2 - yJ)**2)
c       método de Verlet para o asteróide II
        x2aux2 = x2
        y2aux2 = y2
        x2 = 2*x2 - x2aux - alpha*x2*deltat**2/r2**3 - (alpha/1000)*
     +      (x2 - xJ)*deltat**2/r2J**3
        y2 = 2*y2 - y2aux - alpha*y2*deltat**2/r2**3 - (alpha/1000)*
     +      (y2 - yJ)*deltat**2/r2J**3
        x2aux = x2aux2
        y2aux = y2aux2
c       distância do asteróide III ao Sol
        r3 = dsqrt(x3**2 + y3**2)
c       distância do asteróide III a Júpiter
        r3J = dsqrt((x3 - xJ)**2 + (y3 - yJ)**2)
c       método de Verlet para o asteróide III
        x3aux2 = x3
        y3aux2 = y3
        x3 = 2*x3 - x3aux - alpha*x3*deltat**2/r3**3 - (alpha/1000)*
     +      (x3 - xJ)*deltat**2/r3J**3
        y3 = 2*y3 - y3aux - alpha*y3*deltat**2/r3**3 - (alpha/1000)*
     +      (y3 - yJ)*deltat**2/r3J**3
        x3aux = x3aux2
        y3aux = y3aux2
c       distância de Júpiter ao Sol
        rJ = dsqrt(xJ**2 + yJ**2)
c       método de Verlet para Júpiter
        xJaux2 = xJ
        yJaux2 = yJ
        xJ = 2*xJ - xJaux - alpha*xJ*deltat**2/rJ**3
        yJ = 2*yJ - yJaux - alpha*yJ*deltat**2/rJ**3
        xJaux = xJaux2
        yJaux = yJaux2
        T = T + deltat
        write(6, 100)x1, y1
        write(7, 100)x2, y2
        write(8, 100)x3, y3
        write(9, 100)xJ, yJ
        goto 20
      end if
      close(6)
      close(7)
      close(8)
      close(9)

 100  format(F7.2, F7.2)

      end
