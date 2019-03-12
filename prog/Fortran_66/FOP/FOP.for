C001                                                                    001 ---> ярп. 232
      PROGRAM FOP
C
      DIMENSION PSI(600), PS(600), RK(20),
     * ALFA(100), ALOP(100), G(100), V(100)
C
      INTEGER F, F1, LNX(5), IBR(3), IN(3),
     * LL(9), NQ(3), LM(6)
C
      INTEGER*2 NX(600), NUM(600), NUMO(600),
     * MB(1000), MBO(600), MBB(600),
     * NP(200), MP(200), MGRP(200),
     * NXR(100, 2), NOP(100, 2), IK(20),
C
     * NUMR(600), NK(9),
     * NKR(600), NKL(600), NR(100,2),
     * IX(20000)
     
     
      REAl*8 AR8(10)
      LOGICAL*1 TIT(80)
      COMMON IK, RK, MP, MB
      
      COMMON /MASIV/
     * PSI, PS, ALFA, ALOP, G, V,
     * IBR, IN, LNX, LL, MQ, LM,
     * IX, NX, NUM, NUMO, NP, MGRP, MBO, MBB,
C002                                                                    002 ---> ярп. 233
     * NKR, NKL, NR
     
      COMMON /PARAM/
     * W, W0, PRG, CR, CROP, VR, GAM, EMP, QIV, EPS, E,
     * F, N, N0, NMAX, L, IL, IKL,
     * IC, LR, LROP, MAXLR, ID, ID2,
     * NGR1, MIR, MXNGR
     
      EQUIVALENCE (NUMR(1), PS(1)),
C	  бНГЛНФМНЕ ПЕЬЕМХЕ НЬХАЙХ: NXR(1) -> NXR(1,1); NOP(1) -> NOP(1,1)
     * (MB(601), NXR(1)),(MB(801), NOP(1)),
     * (IK(11), NK(1))
     
      DATA AR8/'FOP    ', 'SCONT    ', 'NMIN1    ',
     * 'NMIN2    ', 'NMIN3    ', 'KLOP    ',
     * 'LOKOP    ', 'SUMR    ', 'SUMKL    ',
     * 'SUMLOK    '/
     
      DATA KXLIX, MAXN, MAXL, MAXGR
     * /20000, 200, 600, 600/

C	  ббнд сопюбкъчыху оюпюлерпнб IK, RK
      MAXLR = 100 
      NF9 = 9 
      NF10 = 10 
      CALL DIM (NF10, 2, TIT, L, N, NUM, IX,
     * MAXL, MAXN, MXLIX, 1, 899)
      I3 = 0
      DO 16 I = 1, N
      IF (I.NE.1) I3 = I2
      I2 = 0
      DO 50 J = 1, L
      I6 = N*(J-1) + I
      I1 = IX(I6)
      IF (I1.GT.I2) I2 = I1
   50 CONTINUE 
   16 MGRP(I) = I2 - I3
      WXNGR = 0
C003                                                                    003 ---> ярп. 234
      DO 51 I = 1, N
   51 MXNGR = MXNGR + MGRP(I)  
      IF (MXNGR.LE.MAXGR) GOTO 65
      PRINT 126
      GOTO 99
   65 CONTINUE
      NMAX = N
    1 CONTINUE

C	  ббнд люянй MB, MP 
      CALL MASK (L, N, .FALSE., Iя, LM, NUMR, 899, 81)
      IF (IC.GT.0) GOTO 2
      PRINT 138
      GOTO 41

C     опхябнемхе оюпюлерпюл 
C     гмювемхи он слнквюмхч 
    2 CONTINUE
      IF (IK(1).LT.0.OR.IK(1).GE.5) IK(1) = 1
      IF (IK(2).LE.0.OR.IK(2).GT.50) IK(2) = 20
      IF (IK(3).LT.0.OR.IK(3).GT.4) IK(3) = 0
      IF (IK(4).LT.0) IK(4) = 0
      IF (IK(5).LT.0) IK(5) = 0
      IF (IK(6).LE.0.OR.IK(6).GT.10) IK(6) = 1
      IF (IK(10).LT.0.OR.IK(10).GT.9) IK(10) = 0
      IF (RK(1).LT.0.0001.OR.RK(1).GT.0.5)
     * RK(1) = 0.1
      IF(RK(2).LT.0.1.OR.RK(2).GT.30.0) RK(2)=2. 
      IF(RK(3).LE.0.OR.RK(3).GT.0.5) RK(3) = 0.5
      IF(RK(4).LT.0.0.OR.RK(4).GT.1.0) RK(4) = 0.0
      IF(RK(5).EQ.2.) GOTO 3
      IF(RK(5).LE.0.0.OR.RK(5).GT.1.) RK(5) = 0.5
    3 CONTINUE
      F = IK(1)
C004                                                                    004 ---> ярп. 235
      LR = IK(2)
      NISKL = IK(3)
      IDUBL = IK(4)
      KLAS = IK(5)
      NDOB = IK(6)
      IKL = IK(10)
      EPS = RK(1)
      WO = RK(2)
      QIV = RK(3)
      ECR = RK(4)
      LROP = LR
      E = 1.0 - 1.5 + EPS
      DO 66 I=1,9
      IF (NK(I).NE.0) GOTO 68
   66 CONTINUE
      PRINT 133
   71 NK(1) = 1
      DO 67 I=2,9
   67 NK(I) = 2
      PRINT 134
      GOTO 75
   68 DO 72 I=1,9
      IF (NK(I).EQ.1) GOTO 73
   72 DO 74 I=1,9
      IF (NK(I).EQ.2) GOTO 75
   74 CONTINUE
      PRINT 136
      GOTO 71
C005                                                                    005 ---> ярп. 236
   75 CONTINUE
      DO 76 I=1,9
      IF (NK(I).GE.0.OR.NK(I).LE.2) GOTO 76
      PRINT 135
      GOTO 77
   76 CONTINUE
   77 CONTINUE
      PRINT 104
      PRINT 130, F, LR, NISKL
      PRINT 131, IDUBL, KLAS, NDOB, AR8(IKL+1), IKL
      PRINT 132, EPS, WO, QIV, ECR, GAM
      PRINT 116
      DO 61 I=1,N
   61 NUMR(I) = -MGRP(I)
      PRINT 120
      PRINT 121, (I, NUMR(I), I=1,N)
      IBR(1) = 0
      IBR(2) = 0
      IBR(3) = 0
      NO = LM(1)
      CROP = 1.0E+70
      IL = 0
      IF (IKL.EQ.5.OR.IKL.EQ.8) GOTO 205
      IF (IKL.EQ.6.OR.IKL.EQ.9) GOTO 206
      PRINT 150
      CALL FORMAS(841)
      CALL ISKL(841)
      IF (F.LE.1.OR.IBR(3).NE.0) GOTO 9
      PRINT 106
      PRINT 107
      PRINT 102
C006                                                                    006 ---> ярп. 237
C     бшвхякемхе нанаы╗ммнцн онпрперю
    9 ISP = 0
   22 ISP = ISP + 1
      CALL SGRAD(823, 824, 841)
   23 CALL VIBR(822, 841)
   24 CALL DOBAV(825, 822, 841)
   25 IF (IBR(3).GT.0) GOTO 27
      IF (F.GE.2) PRINT 108
      GOTO 5
   27 CONTINUE
      DO 30 IS=1,2
      IF (IBR(IS).EQ.0) GOTO 30
      I6 = IS + 1
      L1 = LNX(I6) - IBR(IS) + 1
      L2 = LNX(I6)
      DO 33 J = L1,L2
      I1 = NX(J)
      CKX = 0.0
      DO 43 I=1,N
      I2 = NP(I)
      I5 = I1 + I2
      I3 = IX(I5)
   34 CKX = CKX + PSI(I3)
      CKX = CKX - PRG
      IF (IS.EQ.2) GO TO 32
      IF (CKX.LE.0.0) GO TO 33
      GO TO 31
   32 IF (CKX.GT.0.0) GOTO 33
   31 IF (L1.EQ.J) GO TO 35
      NX(J) = NX(L1)
      NX(L1) = I1
   35 IBR(IS) = IBR(IS) - 1
C007                                                                    007 ---> ярп. 238
      L1 = L1 + 1
   33 CONTINUE
   30 CONTINUE
      IB1 = IB
      IB = IBR(1) + IBR(2)
      IBR(3) = IB
      IF (IB.EQ.IB1) GOTO 4
      WO = 1.E 70
      GOTO 22
    4 IF (IBR(3).LE.0) GOTO 6
      PRINT 105
      DO 6 IS=1,2
      IF (IBR(IS).EQ.0) GOTO 6
      K = 0
      I1 = IS + 1
      I2 = LNX(I1) - IBR(IS) + 1
      I3 = LNX(I1)
      DO 7 I = I2,I3
      K = K + 1
      I5 = NX(I)/NMAX + 1
    7 NUMR(K) = NUM(I5)
      PRINT 103,IS,(NUMR(I), I = 1, K)
    6 CONTINUE
      PRINT 116
    5 CONTINUE
      IF (F.GE.1) PRINT 117,ISP,LR,W,IBR
      W0 = W
      LROP = LR
      DO 15 J = 1,LR
      NOP(J, 1) = NXR(J, 1)
      NOP(J, 2) = NXR(J, 2)
   15 ALOP(J) = ALFA(J)
C008                                                                    008 ---> ярп. 239
C     бшанп пефхлю опнцпюллш FOP
      IF (IKL.NE.0) GO TO 201
      CALL EXAM
      PRINT 116
      GO TO 41
  201 IF (IKL.NE.1) GO TO 202
      CALL EXAM
      CALL SCONT
      CALL CRIT
      GOTO 41
  202 IF (IKL.LT.2.OR.IKL.GT.4) GO TO 207
      CALL EXAM
      CALL NMIN
      N = N0
      GOTO 41
  207 IF (IKL.NE.7) GOTO 210
      IF (F.GE.2) CALL EXAM
      CALL SUMR
      IF (F.GE.2) CALL EXAM
      GOTO 41
  205 CALL KLOP(NF9)
      GOTO 41
  206 CALL LOKOP
      GOTO 41
  210 CONTINUE
      PRINT 137
  211 CONTINUE
   41 N = NMAX
      GOTO 1
   99 CONTINUE
      PRINT 123
      STOP
C009                                                                    009 ---> ярп. 240
  102 FORMAT(3X, 'VIBR: хяйкчвемхе бейрнпнб,'/
     * 3X, 'опеоърярбсчыху пюгдекемхч')
C
      END PROGRAM FOP
C
