      program sistemasolar

      implicit real*8 (a-h, o-z)

      alpha = 4.d0*dacos(-1.d0)**2
c     condições iniciais de todos os planetas
      vMex0 = 0.d0
      vMey0 = dsqrt(alpha/0.39d0)
      xMe = 0.39d0
      yMe = 0.d0
      vVx0 = 0.d0
      vVy0 = dsqrt(alpha/0.72d0)
      xV = 0.72d0
      yV = 0.d0
      vTx0 = 0.d0
      vTy0 = dsqrt(alpha)
      xT = 1.d0
      yT = 0.d0
      vMax0 = 0.d0
      vMay0 = dsqrt(alpha/1.52d0)
      xMa = 1.52d0
      yMa = 0.d0
      VJx0 = 0.d0
      vJy0 = dsqrt(alpha/5.2d0)
      xJ = 5.2d0
      yJ = 0.d0
      vSx0 = 0.d0
      vSy0 = dsqrt(alpha/9.24d0)
      xS = 9.24d0
      yS = 0.d0
      vUx0 = 0.d0
      vUy0 = dsqrt(alpha/19.19d0)
      xU = 19.19d0
      yU = 0.d0
      vNx0 = 0.d0
      vNy0 = dsqrt(alpha/30.06d0)
      xN = 30.06d0
      yN = 0.d0
      vPx0 = 0.d0
      vPy0 = dsqrt(alpha/39.53d0)
      xP = 39.53d0
      yP = 0.d0
      deltat = 0.01d0
      T = 0.d0
      open(11, file='mercurio')
      open(12, file='venus')
      open(13, file='terra')
      open(14, file='marte')
      open(15, file='jupiter')
      open(16, file='saturno')
      open(17, file='urano')
      open(18, file='netuno')
      open(19, file='plutao')
      write(11, 100)xMe, yMe
      write(12, 100)xV, yV
      write(13, 100)xT, yT
      write(14, 100)xMa, yMa
      write(15, 100)xJ, yJ
      write(16, 100)xS, yS
      write(17, 100)xU, yU
      write(18, 100)xN, yN
      write(19, 100)xP, yP      

c     passo inicial do método de Verlet para todos os planetas
      xMeaux = xMe
      yMeaux = yMe
      xMe = xMe + vMex0*deltat
      yMe = yMe + vMey0*deltat
      xVaux = xV
      yVaux = yV
      xV = xV + vVx0*deltat
      yV = yV + vVy0*deltat
      xTaux = xT
      yTaux = yT
      xT = xT + vTx0*deltat
      yT = yT + vTy0*deltat
      xMaaux = xMa
      yMaaux = yMa
      xMa = xMa + vMax0*deltat
      yMa = yMa + vMay0*deltat
      xJaux = xJ
      yJaux = yJ
      xJ = xJ + vJx0*deltat
      yJ = yJ + vJy0*deltat
      T = T + deltat
      xSaux = xS
      ySaux = yS
      xS = xS + vSx0*deltat
      yS = yS + vSy0*deltat
      xUaux = xU
      yUaux = yU
      xU = xU + vUx0*deltat
      yU = yU + vUy0*deltat
      xNaux = xN
      yNaux = yN
      xN = xN + vNx0*deltat
      yN = yN + vNy0*deltat
      xPaux = xP
      yPaux = yP
      xP = xP + vPx0*deltat
      yP = yP + vPy0*deltat
      write(11, 100)xMe, yMe
      write(12, 100)xV, yV
      write(13, 100)xT, yT
      write(14, 100)xMa, yMa
      write(15, 100)xJ, yJ
      write(16, 100)xS, yS
      write(17, 100)xU, yU
      write(18, 100)xN, yN
      write(19, 100)xP, yP 

 10   if (T.le.280) then
c       cálculo da distância do planeta ao Sol e aplicação do método de Verlet
c       para cada planeta
        rMe = dsqrt(xMe**2 + yMe**2)
        xMeaux2 = xMe
        yMeaux2 = yMe
        xMe = 2*xMe - xMeaux - alpha*xMe*deltat**2/rMe**3
        yMe = 2*yMe - yMeaux - alpha*yMe*deltat**2/rMe**3
        xMeaux = xMeaux2
        yMeaux = yMeaux2
        rV = dsqrt(xV**2 + yV**2)
        xVaux2 = xV
        yVaux2 = yV
        xV = 2*xV - xVaux - alpha*xV*deltat**2/rV**3
        yV = 2*yV - yVaux - alpha*yV*deltat**2/rV**3
        xVaux = xVaux2
        yVaux = yVaux2
        rT = dsqrt(xT**2 + yT**2)
        xTaux2 = xT
        yTaux2 = yT
        xT = 2*xT - xTaux - alpha*xT*deltat**2/rT**3
        yT = 2*yT - yTaux - alpha*yT*deltat**2/rT**3
        xTaux = xTaux2
        yTaux = yTaux2
        rMa = dsqrt(xMa**2 + yMa**2)
        xMaaux2 = xMa
        yMaaux2 = yMa
        xMa = 2*xMa - xMaaux - alpha*xMa*deltat**2/rMa**3
        yMa = 2*yMa - yMaaux - alpha*yMa*deltat**2/rMa**3
        xMaaux = xMaaux2
        yMaaux = yMaaux2
        rJ = dsqrt(xJ**2 + yJ**2)
        xJaux2 = xJ
        yJaux2 = yJ
        xJ = 2*xJ - xJaux - alpha*xJ*deltat**2/rJ**3
        yJ = 2*yJ - yJaux - alpha*yJ*deltat**2/rJ**3
        xJaux = xJaux2
        yJaux = yJaux2
        rS = dsqrt(xS**2 + yS**2)
        xSaux2 = xS
        ySaux2 = yS
        xS = 2*xS - xSaux - alpha*xS*deltat**2/rS**3
        yS = 2*yS - ySaux - alpha*yS*deltat**2/rS**3
        xSaux = xSaux2
        ySaux = ySaux2
        rU = dsqrt(xU**2 + yU**2)
        xUaux2 = xU
        yUaux2 = yU
        xU = 2*xU - xUaux - alpha*xU*deltat**2/rU**3
        yU = 2*yU - yUaux - alpha*yU*deltat**2/rU**3
        xUaux = xUaux2
        yUaux = yUaux2
        rN = dsqrt(xN**2 + yN**2)
        xNaux2 = xN
        yNaux2 = yN
        xN = 2*xN - xNaux - alpha*xN*deltat**2/rN**3
        yN = 2*yN - yNaux - alpha*yN*deltat**2/rN**3
        xNaux = xNaux2
        yNaux = yNaux2
        rP = dsqrt(xP**2 + yP**2)
        xPaux2 = xP
        yPaux2 = yP
        xP = 2*xP - xPaux - alpha*xP*deltat**2/rP**3
        yP = 2*yP - yPaux - alpha*yP*deltat**2/rP**3
        xPaux = xPaux2
        yPaux = yPaux2
        T = T + deltat
        write(11, 100)xMe, yMe
        write(12, 100)xV, yV
        write(13, 100)xT, yT
        write(14, 100)xMa, yMa
        write(15, 100)xJ, yJ
        write(16, 100)xS, yS
        write(17, 100)xU, yU
        write(18, 100)xN, yN
        write(19, 100)xP, yP 
        goto 10
      end if
      close(11)
      close(12)
      close(13)
      close(14)
      close(15)
      close(16)
      close(17)
      close(18)
      close(19)

 100  format(F7.2, F7.2)

      end
