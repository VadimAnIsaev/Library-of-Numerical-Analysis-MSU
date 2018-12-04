!***************************************************!
!  LU-PAЗЛOЖEHИE METOДOM ГAYCCA C BЫБOPOM BEДYЩEГO  1
!  ЭЛEMEHTA ПO CTOЛБЦY И OЦEHKA ЧИCЛA OБYCЛOBЛEH-   !
!  HOCTИ ЛEHTOЧHOЙ MATPИЦЫ A.                       !
!***************************************************!

SUBROUTINE AFB2C(A,MA,N,ML,MU,NLEAD,RCOND,Z,IERR)

         INTEGER IERR,MA,ML,MU,N,NLEAD
         REAL C1ABS,RCOND
         COMPLEX A,Z
         DIMENSION A(MA,1),NLEAD(1),Z(1)
         EXTERNAL UTAFSI
         INTEGER        I,I0,I1,J,J0,J1,JJ,JU,JZ,K,KB,KP1,L,LL,LM,M,MM,NM1
         REAL ANORM,S,SM,X,YNORM
         COMPLEX EK,T,WK,WKM
         C1ABS(T) = ABS(REAL(T)) + ABS(AIMAG(T))
       RCOND = 0.0
!
!  YCTAHOBЛEHИE KOHTPOЛЯ HA BOЗMOЖHOCTЬ ПEPEПOЛHEHИЯ
       X = 0.0
   10  CONTINUE
!
!  ПPOBEPKA ПPABИЛЬHOCTИ ЗAДAHИЯ ПAPAMETPOB M И N
       IF (N .GE. 1 .AND. MA .GE. 1) GO TO 20
          IERR = 65
          CALL UTAFSI(1HA,1HF,1HB,1H2,1HC,IERR)
          GO TO 890
   20  CONTINUE
       IERR = 0
       M = ML + MU + 1
       DO I = 1, N
          J0 = MAX0(1,I-ML)
          J1 = MIN0(N,I+MU)
          DO J = J0, J1
             K = J - I + ML + 1
             Z(J) = A(I,K)
          END DO
          DO J = J0, J1
             A(I,J) = Z(J)
          END DO
       END DO
C
C  BЫЧИCЛEHИE 1-HOPMЫ MATPИЦЫ A
       ANORM = 0.
       DO J = 1, N
          I0 = MAX0(1,J-MU)
          I1 = MIN0(N,J+ML)
          DO I = I0, I1
             K = I - J + M
             Z(K) = A(I,J)
          END DO
          S = 0.0
          DO I = I0, I1
             K = I - J + M
             A(K,J) = Z(K)
             S = S + C1ABS(Z(K))
          END DO
          IF (S .GT. ANORM) ANORM = S
       END DO
       J0 = MU + 2
       J1 = MIN0(N,M) - 1
       IF (J1 .LT. J0) GO TO 110
          DO JZ = J0, J1
             I0 = M + 1 - JZ
             DO I = I0, ML
                A(I,JZ) = (0.0,0.0)
             END DO
          END DO
  110  CONTINUE
       JZ = J1
       JU = 0
C
C  LU-PAЗЛOЖEHИE MATPИЦЫ A
       NM1 = N - 1
       IF (NM1 .LT. 1) GO TO 310
       DO K = 1, NM1
             KP1 = K + 1
             JZ = JZ + 1
             IF (JZ .GT. N) GO TO 130
                IF (ML .LT. 1) GO TO 130
                   DO I = 1, ML
                      A(I,JZ) = (0.0,0.0)
                   END DO
  130           CONTINUE
C
C  HAXOЖДEHИE ИHДEKCA BEДYЩEГO ЭЛEMEHTA L
                LM = MIN0(ML,N-K)
                L = M
                S = C1ABS(A(M,K))
                I0 = M + 1
                I1 = M + LM
                DO I = I0, I1
                   IF (C1ABS(A(I,K)) .LE. S) GO TO 140
                      L = I
                      S = C1ABS(A(I,K))
  140              CONTINUE
                END DO
                NLEAD(K) = L + K - M
                IF (CABS(A(L,K)) .EQ. 0.0) GO TO 280
C
C  ПEPECTAHOBKA,ECЛИ TPEБYETCЯ
                   IF (L .EQ. M) GO TO 160
                      T = A(L,K)
                      A(L,K) = A(M,K)
                      A(M,K) = T
  160              CONTINUE
C
C  BЫЧИCЛEHИE MHOЖИTEЛEЙ
                   T = (-1.0,0.0)/A(M,K)
                   I0 = M + 1
                   I1 = M + LM
                   I = MOD(LM,5) + M
                   IF (I .EQ. M) GO TO 180
                      DO J = I0, I
                         A(J,K) = T*A(J,K)
                      END DO
  180              CONTINUE
                   I = I + 1
                   IF (I .GT. I1) GO TO 200
                      DO J = I, I1, 5
                         A(J,K) = T*A(J,K)
                         A(J+1,K) = T*A(J+1,K)
                         A(J+2,K) = T*A(J+2,K)
                         A(J+3,K) = T*A(J+3,K)
                         A(J+4,K) = T*A(J+4,K)
                      END DO
  200              CONTINUE
!
!  ГAYCCOBO ИCKЛЮЧEHИE
                   JU = MIN0(MAX0(JU,MU+NLEAD(K)),N)
                   MM = M
                   IF (JU .LT. KP1) GO TO 270
                      DO J = KP1, JU
                         L = L - 1
                         MM = MM - 1
                         T = A(L,J)
                         IF (L .EQ. MM) GO TO 210
                            A(L,J) = A(MM,J)
                            A(MM,J) = T
  210                    CONTINUE
                         I0 = M + 1
                         I1 = M + LM
                         I = MOD(LM,4) + M
                         IF (I .EQ. M) GO TO 230
                            DO JJ = I0, I
                               KB = JJ + MM - M
                               A(KB,J) = A(KB,J) + T*A(JJ,K)
                            END DO
  230                    CONTINUE
                         I = I + 1
                         IF (I .GT. I1) GO TO 250
                            DO JJ = I, I1, 4
                               KB = JJ + MM - M
                               A(KB,J) = A(KB,J) + T*A(JJ,K)
                               A(KB+1,J) = A(KB+1,J) + T*A(JJ+1,K)
                               A(KB+2,J) = A(KB+2,J) + T*A(JJ+2,K)
                               A(KB+3,J) = A(KB+3,J) + T*A(JJ+3,K)
                            END DO
  250                    CONTINUE
                      END DO
  270              CONTINUE
                   GO TO 290
  280        CONTINUE
             IERR = -K
  290     CONTINUE
       END DO
  310  CONTINUE
       IF (CABS(A(M,N)) .EQ. 0.0) IERR = -N
       NLEAD(N) = N
       IF (IERR .EQ. 0) GO TO 320
          CALL UTAFSI(1HA,1HF,1HB,1H2,1HC,IERR)
          GO TO 820
  320  CONTINUE
!
!  BЫЧИCЛEHИE RCOND-BEЛИЧИHЫ,OБPATHOЙ ЧИCЛY OБYCЛOBЛEHHOCTИ MATPИЦЫ A.
!  ДЛЯ BЫЧИCЛEHИЯ HOPMЫ MATPИЦЫ INVERSE(A) PEШAETCЯ CИCTEMA A*Z=Y,
!  ГДE Y-PEШEHИE CИCTEMЫ TRANS(A)*Y=E;KOMПOHEHTЫ BEKTOPA E BЫБИPAЮTCЯ
!  TAKИM OБPAЗOM,ЧTOБЫ MAKCИMИЗИPOBATЬ HOPMY BEKTOPA W,ГДE W-PEШEHИE
!  CИCTEMЫ TRANS(U)*W=E.
!
!  PEШEHИE CИCTEMЫ TRANS(U)*W=E OДHOBPEMEHHO C BЫБOPOM E
       EK = (1.0,0.0)
       DO J = 1, N
          Z(J) = (0.0,0.0)
       END DO
       JU = 0
       DO K = 1, N
          YNORM = C1ABS(EK)
          IF (C1ABS(Z(K)) .NE. 0.0)
     1       EK = -Z(K)*CMPLX(YNORM/C1ABS(Z(K)),0.0)
          IF (C1ABS(EK-Z(K)) .LE. C1ABS(A(M,K))) GO TO 400
             S = C1ABS(A(M,K))/C1ABS(EK-Z(K))
             ASSIGN 390 TO LL
  340     CONTINUE
          I = MOD(N,5)
          T = CMPLX(S,0.0)
          IF (I .EQ. 0) GO TO 360
             DO JJ = 1, I
                Z(JJ) = T*Z(JJ)
             END DO
             IF (N .LT. 5) GO TO 380
  360        CONTINUE
             I = I + 1
             DO JJ = I, N, 5
                Z(JJ) = T*Z(JJ)
                Z(JJ+1) = T*Z(JJ+1)
                Z(JJ+2) = T*Z(JJ+2)
                Z(JJ+3) = T*Z(JJ+3)
                Z(JJ+4) = T*Z(JJ+4)
             END DO
  380     CONTINUE
          GO TO LL,(390,540,590,630,680,720,730,810)
  390     CONTINUE
          EK = T*EK
  400     CONTINUE
          WK = EK - Z(K)
          WKM = -EK - Z(K)
          S = C1ABS(WK)
          SM = C1ABS(WKM)
          IF (CABS(A(M,K)) .EQ. 0.0) GO TO 410
             WK = WK/A(M,K)
             WKM = WKM/A(M,K)
             GO TO 420
  410     CONTINUE
          WK = (1.0,0.0)
          WKM = (1.0,0.0)
  420     CONTINUE
          KP1 = K + 1
          JU = MIN0(MAX0(JU,MU+NLEAD(K)),N)
          MM = M
          IF (KP1 .GT. JU) GO TO 460
             DO J = KP1, JU
                MM = MM - 1
                SM = SM + C1ABS(Z(J) + WKM*CONJG(A(MM,J)))
                Z(J) = Z(J) + WK*CONJG(A(MM,J))
                S = S + C1ABS(Z(J))
             END DO
             IF (S .GE. SM) GO TO 450
                T = WKM - WK
                WK = WKM
                MM = M
                DO J = KP1, JU
                   MM = MM - 1
                   Z(J) = Z(J) + T*CONJG(A(MM,J))
                END DO
  450        CONTINUE
  460     CONTINUE
          Z(K) = WK
       END DO
       ASSIGN 530 TO LL
  480  CONTINUE
       S = 0.0
       I = MOD(N,6)
       IF (I .EQ. 0) GO TO 500
          DO JJ = 1, I
             S = S + C1ABS(Z(JJ))
          END DO
          IF (N .LT. 6) GO TO 520
  500     CONTINUE
          I = I + 1
          DO JJ = I, N, 6
             S = S + C1ABS(Z(JJ)) + C1ABS(Z(JJ+1)) + C1ABS(Z(JJ+2))
     1            + C1ABS(Z(JJ+3)) + C1ABS(Z(JJ+4)) + C1ABS(Z(JJ+5))
          END DO
  520  CONTINUE
       S = 1.0/S
       GO TO LL,(530,620,710,800)
  530  CONTINUE
       ASSIGN 540 TO LL
       GO TO 340
  540  CONTINUE
C
C  PEШEHИE CИCTEMЫ TRANS(L)*Y=W
       DO KB = 1, N
          K = N + 1 - KB
          LM = MIN0(ML,N-K)
          IF (K .GE. N) GO TO 580
             I0 = M + 1
             I1 = M + LM
             I = MOD(LM,5) + M
             IF (I .EQ. M) GO TO 560
                DO JJ = I0, I
                   KP1 = JJ + K - M
                   Z(K) = Z(K) + CONJG(A(JJ,K))*Z(KP1)
                END DO
  560        CONTINUE
             I = I + 1
             IF (I .GT. I1) GO TO 580
                DO 570 JJ = I, I1, 5
                   KP1 = JJ + K - M
                   Z(K) = Z(K) + CONJG(A(JJ,K))*Z(KP1) + CONJG(A(JJ+
     1                    1,K))*Z(KP1+1) + CONJG(A(JJ+2,K))*Z(KP1+2)
     2                     + CONJG(A(JJ+3,K))*Z(KP1+3) + CONJG(A(JJ+
     3                    4,K))*Z(KP1+4)
  570           CONTINUE
  580        CONTINUE
             IF (C1ABS(Z(K)) .LE. 1.0) GO TO 600
                S = 1.0/C1ABS(Z(K))
                ASSIGN 590 TO LL
                GO TO 340
  590     CONTINUE
  600     CONTINUE
          L = NLEAD(K)
          T = Z(L)
          Z(L) = Z(K)
          Z(K) = T
       END DO
       ASSIGN 620 TO LL
       GO TO 480
  620  CONTINUE
       ASSIGN 630 TO LL
       GO TO 340
  630  CONTINUE
       YNORM = 1.
C
C  PEШEHИE CИCTEMЫ L*V=Y И BЫЧИCЛEHИE HOPMЫ BEKTOPA Y
       DO K = 1, N
          L = NLEAD(K)
          T = Z(L)
          Z(L) = Z(K)
          Z(K) = T
          LM = MIN0(ML,N-K)
          IF (K .GE. N) GO TO 670
             I0 = M + 1
             I1 = M + LM
             I = MOD(LM,4) + M
             IF (I .EQ. M) GO TO 650
                DO JJ = I0, I
                   KP1 = JJ + K - M
                   Z(KP1) = Z(KP1) + T*A(JJ,K)
                END DO
  650        CONTINUE
             I = I + 1
             IF (I .GT. I1) GO TO 670
                DO JJ = I, I1, 4
                   KP1 = JJ + K - M
                   Z(KP1) = Z(KP1) + T*A(JJ,K)
                   Z(KP1+1) = Z(KP1+1) + T*A(JJ+1,K)
                   Z(KP1+2) = Z(KP1+2) + T*A(JJ+2,K)
                   Z(KP1+3) = Z(KP1+3) + T*A(JJ+3,K)
                END DO
  670        CONTINUE
             IF (C1ABS(Z(K)) .LE. 1.0) GO TO 690
                S = 1.0/C1ABS(Z(K))
                ASSIGN 680 TO LL
                GO TO 340
  680     CONTINUE
          YNORM = S*YNORM
  690     CONTINUE
       END DO
       ASSIGN 710 TO LL
       GO TO 480
  710  CONTINUE
       ASSIGN 720 TO LL
       GO TO 340
  720  CONTINUE
       YNORM = S*YNORM
C
C  PEШEHИE CИCTEMЫ U*Z=V
       DO KB = 1, N
          K = N + 1 - KB
          IF (C1ABS(Z(K)) .LE. C1ABS(A(M,K))) GO TO 740
             S = C1ABS(A(M,K))/C1ABS(Z(K))
             ASSIGN 730 TO LL
             GO TO 340
  730     CONTINUE
          YNORM = S*YNORM
  740     CONTINUE
          IF (CABS(A(M,K)) .NE. 0.0) Z(K) = Z(K)/A(M,K)
          IF (CABS(A(M,K)) .EQ. 0.0) Z(K) = (1.0,0.0)
          T = -Z(K)
          LM = MIN0(K,M) - 1
          IF (LM .LT. 1) GO TO 780
             I0 = M - LM
             I1 = M - 1
             I = MOD(LM,4) + I0 - 1
             IF (I .LT. I0) GO TO 760
                DO JJ = I0, I
                   KP1 = JJ + K - M
                   Z(KP1) = Z(KP1) + T*A(JJ,K)
  750           END DO
  760        CONTINUE
             I = I + 1
             IF (I .GT. I1) GO TO 780
                DO JJ = I, I1, 4
                   KP1 = JJ + K - M
                   Z(KP1)=Z(KP1)+T*A(JJ,K)
                   Z(KP1+1)=Z(KP1+1)+T*A(JJ+1,K)
                   Z(KP1+2)=Z(KP1+2)+T*A(JJ+2,K)
                   Z(KP1+3)=Z(KP1+3)+T*A(JJ+3,K)
                END DO
  780        CONTINUE
       END DO
          ASSIGN 800 TO LL
          GO TO 480
  800  CONTINUE
       ASSIGN 810 TO LL
       GO TO 340
  810  CONTINUE
       YNORM=S*YNORM
       IF(ANORM.NE.0.) RCOND=YNORM/ANORM
  820  CONTINUE
       DO J=1,N
          I0=MAX0(1,J-MU-ML)
          I1=MIN0(N,J+ML)
          DO I=I0,I1
             K=I-J+M
             Z(I)=A(K,J)
             A(K,J)=(0.0,0.0)
          END DO
          DO I=I0,I1
             A(I,J)=Z(I)
          END DO
       END DO
       DO I=1,N
         J0=MAX0(1,I-ML)
         J1=MIN0(N,I+MU+ML)
         DO J=J0,J1
            K=J-I+ML+1
            Z(K)=A(I,J)
            A(I,J)=(0.0,0.0)
         END DO
         DO J=J0,J1
           K=J-I+ML+1
           A(I,K)=Z(K)
         END DO
       END DO
  890  CONTINUE
       RETURN
END SUBROUTINE AFB2C
