C001                                                                    001 ---> ÑÒÐ. 232
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
C002                                                                    002 ---> ÑÒÐ. 233
     * NKR, NKL, NR
     
      COMMON /PARAM/
     * W, W0, PRG, CR, CROP, VR, GAM, EMP, QIV, EPS, E,
     * F, N, N0, NMAX, L, IL, IKL,
     * IC, LR, LROP, MAXLR, ID, ID2,
     * NGR1, MIR, MXNGR
     
      EQUIVALENCE (NUMR(1), PS(1)),
C	  Âîçìîæíîå ðåøåíèå îøèáêè: NXR(1) -> NXR(1,1); NOP(1) -> NOP(1,1)
     * (MB(601), NXR(1)),(MB(801), NOP(1)),
     * (IK(11), NK(1))
     
      DATA AR8/'FOP    ', 'SCONT    ', 'NMIN1    ',
     * 'NMIN2    ', 'NMIN3    ', 'KLOP    ',
     * 'LOKOP    ', 'SUMR    ', 'SUMKL    ',
     * 'SUMLOK    '/
     
      DATA KXLIX, MAXN, MAXL, MAXGR
     * /20000, 200, 600, 600/

C	  ÂÂÎÄ ÓÏÐÀÂËßÞÙÈÕ ÏÀÐÀÌÅÒÐÎÂ IK, RK
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
C003                                                                    003 ---> ÑÒÐ. 234
      DO 51 I = 1, N
   51 MXNGR = MXNGR + MGRP(I)  
      IF (MXNGR.LE.MAXGR) GOTO 65
      PRINT 126
      GOTO 99
   65 CONTINUE
      NMAX = N
    1 CONTINUE

C	  ÂÂÎÄ ÌÀÑÎÊ MB, MP 
      CALL MASK (L, N, .FALSE., IÑ, LM, NUMR, 899, 81)
      IF (IC.GT.0) GOTO 2
      PRINT 138
      GOTO 41

C     ÏÐÈÑÂÎÅÍÈÅ ÏÀÐÀÌÅÒÐÀÌ 
C     ÇÍÀ×ÅÍÈÉ ÏÎ ÓÌÎË×ÀÍÈÞ 
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
C004                                                                    004 ---> ÑÒÐ. 235
      
C
      END PROGRAM FOP
C