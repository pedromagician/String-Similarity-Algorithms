10 DIM Word_1$(20), Word_2$(20), Z$(20), DLDm(21, 21)
11 POKE 82,0 : CLS

20 ? "Zadaj slovo 1:" : INPUT Word_1$
30 ? "Zadaj slovo 2:" : INPUT Word_2$
40 CLS : EXEC _EQUAL_

13000 PROC _EQUAL_
13040   EXEC _DLD_ : ? Word_1$ : ? Word_2$ : ? "original string DLD:";RESULT : ?
13080   EXEC _JWD_ : ? Word_1$ : ? Word_2$ : ? "original string JWD:";RESULT : ?
13110   Min=LEN(Word_1$) : L=LEN(Word_2$) : IF Min>L THEN Min=L
13120   IF Min>14 THEN Min=14
13130   Word_1$=Word_1$(1,Min) : Word_2$=Word_2$(1,Min)
13170   EXEC _DLD_ : ? Word_1$ : ? Word_2$ : ? "short string DLD:";RESULT : ?
13210   EXEC _JWD_ : ? Word_1$ : ? Word_2$ : ? "short string JWDs:";RESULT : ?
13250 ENDPROC


11000 END
11600 PROC _DLD_ : REM DamerauLevenshteinDistance
11601   REM Word_1$ : Word_2$ : DLDm[]
11602   I=0: J=0 : K=0 : L=0 : M=0 : N=0 : Min=0 : Result=0

11610   M=LEN(Word_1$) : N=LEN(Word_2$)
11620   FOR I=0 TO M : DLDm(I,0)=I : NEXT I
11630   FOR J=0 TO N : DLDm(0,J)=J : NEXT J
11640   FOR J=1 TO N
11650     FOR I=1 TO M
11660       IF Word_1$(I,I) = Word_2$(J,J)
11670         DLDm(I,J) = DLDm(I-1, J-1) : REM no operation required
11680       ELSE
11690         Min = DLDm(I-1, J)+1 : REM delete
11700         K = DLDm(I, J-1)+1   : REM insert
11710         L = DLDm(I-1, J-1)+1 : REM substitution
11720         IF K < Min THEN Min=K
11730         IF L < Min THEN Min=L
11740         DLDm(I,J) = Min

11750         IF I>1 AND J>1 : REM ---> Damerau Levenshtein Distance BEGIN
11760           IF Word_1$(I,I) = Word_2$(J-1,J-1) AND Word_1$(I-1,I-1) = Word_2$(J,J)
11770               Min=DLDm(I,J) : IF Min>(DLDm(I-2,J-2)+1) THEN Min=(DLDm(I-2,J-2)+1)
11780               DLDm(I,J) = Min : REM transposition
11790           ENDIF
11800         ENDIF : REM <--- Damerau Levenshtein Distance END

11810       ENDIF
11820     NEXT I
11830   NEXT J
11840   Result=DLDm(M,N)
11845   REM ? "Damerau Levenshtein Distance=";Result
11850 ENDPROC


12000 PROC _JWD_
11601   REM Word_1$ : Word_2$ : DLDm[]
11602   I=0: J=0 : K=0 : L=0 : M=0 : N=0 : Min=0 : Max=0 : Result=0 : Z$=""

12010   S1=LEN(Word_1$) : S2=LEN(Word_2$)
12020   IF S1>S2 THEN Z$=Word_1$ : Word_1$=Word_2$ : Word_2$=Z$ : M=S1 : S1=S2 : S2=M
12030   J=1: M=0 : N=0 : L=INT(S2/2) : Z$=Word_2$
12040   FOR I=1 TO S1
12050     IF Word_1$(I,I)=Word_2$(J,J) THEN M=M+1: Word_2$(J,J)=" ": GO# JMP_JWD
12060     Max=1 : IF Max<(I-L) THEN Max=I-L
12070     Min=S2 : IF Min>(I+L-1) THEN Min=I+L-1
12080     FOR K=Max TO Min
12090       IF Word_1$(I,I)=Word_2$(K,K) THEN N=N+1: M=M+1: Word_2$(K,K)=" ": IF K>J THEN J=K
12100     NEXT K
12110     #JMP_JWD : IF J<S2 THEN J=J+1
12120   NEXT I
12130   IF M=0
12140     Result=0 : REM jaro distance
12150   ELSE 
12160     N=INT(N/2)
12170     Result=(M/S1+M/S2+((M-N)/M))/3. : REM jaro distance
12180   ENDIF
12190   REM ? "Jaro Distance=";Result
12200   Min=S1 : IF Min>S2 THEN Min=S2
12210   M=Min : IF M>3 THEN M=3
12220   M=M+1 : L=0 : Word_2$=Z$ : IF M>Min THEN M=Min
12230   FOR I=1 TO M
12240     IF Word_1$(I,I)=Word_2$(I,I)
12250       L=L+1
12260     ELSE
12270       EXIT
12280     ENDIF
12290   NEXT I
12300   Result=Result + (L*0.1*(1.0 - Result)) : REM Winkler
12310   REM ? "Jaro Winkler Distance=";Result
12320 ENDPROC
